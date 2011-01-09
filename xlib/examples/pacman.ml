(***********************************************************************)
(*                                                                     *)
(*                           PacMan                                    *)
(*                                                                     *)
(*          Sylvain Conchon, projet Para, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Array;;
open XGraphics;;

(* -- il faut ouvrir la fenetre avant de charger des images -- *)
let _ =
  open_graph "" 560 700;;

set_update_style FlushClipped;;






type brique = Mur | Pastille | Dot | Vide | Sortie;; 
type direction = Droite | Gauche | Haut | Bas;;
type t_plateau = 
    Horiz | Verti | Haut_gauche | Haut_droit | Bas_gauche | Bas_droit | Blanc 
  | Exit | Point | Bonus;;
type etat = Normal | Chasse | En_init;;

(* Quelques constantes *)
let etat_jeu = ref Normal (* etat du jeu *)
and chrono_chasse = ref 0 (* temps pour chasser les monstres *)
and delay = ref 0.001 
and temps_chasse_max = 1000 (* temps maxi de chasse *)
and point_pastille = 50
and point_dot = 10
and max_vie = 30 
and taille_pac = 9
and x_pac_init = 14
and y_pac_init = 7
and pos_ghost_init = [|(14,20);(15,17);(14,16);(15,16)|] (* positions de depart des monstres *)
and nb_dot = 255;; (* nombre de dots et pastilles sur le plateau *)


(* -- Variables Globales -- *)

(* les images *)
let monstres = 
  [|(create_image_from_xpm "images/monstre_bleu.xpm");
    (create_image_from_xpm "images/monstre_red.xpm");
    (create_image_from_xpm "images/monstre_vert.xpm");
    (create_image_from_xpm "images/monstre_viol.xpm")|]

and monstre_chasse = create_image_from_xpm "images/monstre_chasse.xpm"
and img = [|(create_image 20 20);(create_image 20 20);
            (create_image 20 20);(create_image 20 20);|]
and horiz = create_image_from_xpm "images/droit.xpm"
and verti = create_image_from_xpm"images/vert.xpm"
and haut_droit = create_image_from_xpm "images/haut_droit.xpm"
and haut_gauche = create_image_from_xpm  "images/haut_gauche.xpm"
and bas_droit = create_image_from_xpm "images/bas_droit.xpm"
and bas_gauche = create_image_from_xpm "images/bas_gauche.xpm";;
 
let gagne = ref false
and couleur_monstre = ref true ;;

(* le terrain *)
let terrain = create_matrix 32 30 Vide ;;
let plateau = create_matrix 32 30 Blanc ;;
    
let chargement_terrain () = 
  let f = open_in "plateau1.txt" in
  for i = 31 downto 0 do
    for j = 1 to 28 do
      match (input_char f) with 
        '0' ->  
          begin
            terrain.(i).(j)<-Mur;
            plateau.(i).(j)<-Horiz;
          end;    
      | '1' ->
          begin
            terrain.(i).(j)<-Mur;
            plateau.(i).(j)<-Haut_gauche;
          end;
      | '2' ->
          begin
            terrain.(i).(j)<-Mur;
            plateau.(i).(j)<-Haut_droit;
          end;
      | '3' ->
          begin
            terrain.(i).(j)<-Mur;
            plateau.(i).(j)<-Bas_gauche;
          end;
      | '4' ->
          begin
            terrain.(i).(j)<-Mur;
            plateau.(i).(j)<-Bas_droit;
          end;
      | '5' ->
          begin
            terrain.(i).(j)<-Mur;
            plateau.(i).(j)<-Verti;
          end;
      | '.' ->  
          begin
            terrain.(i).(j)<-Dot;
            plateau.(i).(j)<-Point;
          end;   
      | '*' ->
          begin
            terrain.(i).(j)<-Pastille;
            plateau.(i).(j)<-Bonus;
          end;
      | '-' ->
          begin
            terrain.(i).(j)<-Vide;
            plateau.(i).(j)<-Blanc;
          end;
      | 'x' ->
          begin
            terrain.(i).(j)<-Sortie;
            plateau.(i).(j)<-Exit;
          end;
      | _ -> 
          begin
            terrain.(i).(j)<-Vide;
            plateau.(i).(j)<-Blanc;
          end;
    done;
     input_char f;
  done;
  close_in f;;


let dessine_fond () =
  set_color black;
  fill_rect 0 0 560 700;
  for i=0 to 31 do
    for j=1 to 28 do
      match plateau.(i).(j) with
        Point ->
          begin
            set_color yellow;
            fill_circle ((j-1)*20+10) (i*20+10) 2;
          end
      | Bonus -> 
          begin
            set_color yellow;
            fill_circle ((j-1)*20+10) (i*20+10) 6;
          end;
      | Horiz  ->  draw_image horiz ((j-1)*20) (i*20)
      | Verti  ->  draw_image verti ((j-1)*20) (i*20)
      | Haut_gauche -> draw_image haut_gauche ((j-1)*20) (i*20)
      | Haut_droit ->   draw_image haut_droit ((j-1)*20) (i*20)
      | Bas_gauche ->  draw_image bas_gauche ((j-1)*20) (i*20)
      | Bas_droit  ->  draw_image bas_droit ((j-1)*20) (i*20)
      | Exit -> 
          begin
            set_color (rgb 240 240 240);
            fill_rect ((j-1)*20+1) (i*20+8) 18 4;
          end
      | Blanc -> ()
    done;
  done;
  set_color black ;;

let menu() =
  set_color black;
  fill_rect 0 0 560 700;
  moveto 120 400;
  set_color yellow;
  set_font "lucidasans-bolditalic-14"; 
  draw_string "Appuyez sur '1' pour commencer";
  moveto 220 360;
  draw_string "ou 's' pour sortir";
  update();
  let t = ref ' ' in
  while (!t<>'1') && (!t<>'s') do t := read_key() done;
  !t = '1';;  


let au_revoir() = 
  set_color black;
  fill_rect 0 0 560 700;
  moveto 220 400;
  set_color yellow;
  set_font "lucidasans-bolditalic-24"; 
  draw_string "Au revoir...";
  update();
;;

(* pour les monstres *)
let efface_monstres x_m y_m =
  for i=0 to 3 do
    draw_image img.(i) x_m.(i) y_m.(i);
  done;;

let affiche_monstres x_m y_m etat_monstre=
  let draw_monstre_chasse x y = if !couleur_monstre then draw_image monstre_chasse x y in
  
  for i=0 to 3 do
    blit_image img.(i) x_m.(i) y_m.(i)
  done;
  for i=0 to 3 do
    if (etat_monstre.(i)=Chasse) then draw_monstre_chasse x_m.(i) y_m.(i)
    else draw_image monstres.(i) x_m.(i) y_m.(i)
  done;;
  
(* pour le pacman *)
let affiche_pac x y b d_pac= 
    set_color yellow;
    fill_circle x y taille_pac;
    set_color black;
    match d_pac with
      Gauche ->fill_arc (x+3) y (taille_pac+3) taille_pac (150 + b*5) (210 - b*5);
    | Droite ->fill_arc (x-3) y (taille_pac+3) taille_pac (b*5 - 30) (30 - b*5);
    | Haut ->fill_arc x (y-3) taille_pac (taille_pac+3) (60 + b*5) (120 - b*5);
    | Bas ->fill_arc x (y+3) taille_pac (taille_pac+3) (240 + b*5) (300 - b*5);

and efface_pac x y = fill_rect (x-10) (y-10) 20 20;;
        
let affiche_score score =
    set_color black;
    fill_rect 10 680 500 30;
    set_color red;
    moveto 10 680;
    draw_string("Score : "^string_of_int(score));
    set_color black;

and affiche_vie vie = 
  set_color black;
  fill_rect 18 660 100 18;
  for i=0 to (vie-1) do
    set_color yellow;
    fill_circle (18+i*20) 668 taille_pac;
    set_color black;
    fill_arc (15+i*20) 668 (taille_pac+3) taille_pac (-30) 30;
  done;;

let perdu() = 
  moveto 220 340;
  set_color (rgb 255 0 0);
  set_font "lucidasans-bolditalic-24"; 
  set_text_size 30;
  draw_string "PERDU !";
  moveto 120 300;
  set_font "lucidasans-bolditalic-14";
  draw_string "APPUYEZ SUR UNE TOUCHE...";
  update();
  read_key();;


       
(*------ PACMAN -------*)
(* Sans objet pour gagner un peu de temps ...*)      

let nbdot_pac = ref 0;; (* nombre de dots mangees par le PacMan *)

let init_pac x_pac y_pac score vie =
  (* initialisation du terrain de jeu *)
  nbdot_pac := 0;
  chrono_chasse := 0;
  etat_jeu := Normal;
  set_color black;
  affiche_score score;
  affiche_vie vie;
  (x_pac_init*20 +10,y_pac_init*20 +10);;
    
let efface x y x_m y_m = 
  efface_pac x y; 
  efface_monstres x_m y_m ;;

let affiche x y b x_m y_m etat_monstre d_pac= 
    affiche_monstres x_m y_m etat_monstre;
    affiche_pac x y b d_pac;;
    
let chgtdir d_pac =
  let d' = read_key() in
  match d' with
    '4' | 'j'  -> Gauche
  | '6' | 'k' -> Droite
  | '8' | 'a' -> Haut
  | '5' | '2' | 'z' -> Bas
  | '+' -> delay := !delay +. 0.0001;d_pac
  | '-' -> delay := !delay -. 0.0001;d_pac
  | _ -> d_pac;;
  
let maj_case y x score etat_monstre=
  if terrain.(y).(x) <> Vide then
    begin
      sound 10 50 ;
      if terrain.(y).(x)= Pastille then
        begin
          score := !score + point_pastille;
          etat_jeu := Chasse;
          for i=0 to 3 do
              etat_monstre.(i) <- Chasse;
          done;
          chrono_chasse := temps_chasse_max;
          couleur_monstre := true;
        end
      else score := !score + point_dot;
      incr nbdot_pac;
      if !nbdot_pac=nb_dot then gagne := true;
      terrain.(y).(x) <- Vide;
      plateau.(y).(x) <- Blanc;
      affiche_score !score;
    end;;


let incpac = ref (-1) (* utilise pour la bouche du pac *);;
let mouvement_pac x y b score etat_monstre d_pac= 
  let incr_bouche x b =
    if x mod 2 = 0 then
      begin
        if (b=7) || (b=0) then incpac := !incpac * -1;
        (b + !incpac)  
      end
    else b
  and delta = 11 in
  match d_pac with
    Gauche -> 
      begin
        let xx = (x-delta)/20 +1 in
        let yy = y/20 in
        match terrain.(yy).(xx) with
          Pastille | Dot | Vide ->
            begin
              maj_case yy xx score etat_monstre;
              if x=0 then (560,y,incr_bouche x b)  else (x-1,y,incr_bouche x b)
            end 
        | Mur | Sortie -> (x,y,b) ;
      end
  | Droite ->
      begin
        let xx= (x+delta-1)/20 +1 in
        let yy = y/20 in
        match terrain.(yy).(xx) with
          Pastille | Dot | Vide ->
            begin
              maj_case yy xx score etat_monstre;
              if x=560 then (0,y,incr_bouche x b) else (x+1,y,incr_bouche x b);
            end
        | Mur | Sortie -> (x,y,b);
      end
  | Haut ->
      begin
        let xx = x/20 +1 in
        let yy = (y+delta-1)/20 in
        match terrain.(yy).(xx) with
          Pastille | Dot | Vide ->
            begin
              maj_case yy xx score etat_monstre;
              (x,y+1,incr_bouche y b)
            end
        | Mur | Sortie ->(x,y,b);
      end
  | Bas ->
      begin
        let xx = x/20 +1 in
        let yy = (y-delta)/20 in
        match terrain.(yy).(xx) with
          Pastille | Dot | Vide ->
            begin
              maj_case yy xx score etat_monstre; 
              (x,y-1,incr_bouche y b)
            end
        | Mur | Sortie -> (x,y,b);
      end;;


let bouge_pac x y b score etat_monstre d_pac dd_pac=
  let d=ref d_pac in
  if (x+20) mod 20=10 && (y+20) mod 20=10 then
    begin
      match dd_pac with
        Gauche ->if terrain.(y/20).((x-20)/20 +1) <> Mur then d :=dd_pac; 
      | Droite ->if terrain.(y/20).((x+20)/20 +1) <> Mur then d :=dd_pac;
      | Haut ->if terrain.((y+20)/20).(x/20 +1) <> Mur then d :=dd_pac;
      | Bas ->if terrain.((y-20)/20).(x/20 +1) <> Mur then d :=dd_pac;
    end;
  (mouvement_pac x y b score etat_monstre !d,!d)


(* --- MONSTRES --- *)

let num_def = [|0;0;0;0|] (* numero de la position predefinie *)
and max_def = [|34;53;21;19|] (* nombre de pas predefini pour chaque monstre *)
and mvt_def = 
    [|
      [|Gauche;Gauche;Gauche;Gauche;Gauche;Bas;Bas;Bas;Gauche;Gauche;Gauche;Haut;Haut;
        Haut;Haut;Haut;Haut;Haut;Haut;Haut;Gauche;Gauche;Gauche;Gauche;Gauche;Haut;Haut;
        Haut;Haut;Droite;Droite;Droite;Droite;Droite|] ; 

      [|Haut;Haut;Haut;Gauche;Gauche;Gauche;Gauche;Gauche;Gauche;Bas;Bas;Bas;Gauche;Gauche;
        Gauche;Haut;Haut;Haut;Haut;Haut;Haut;Haut;Haut;Haut;Droite;Droite;Droite;Droite;
        Droite;Droite;Droite;Droite;Droite;Droite;Droite;Droite;Droite;Droite;Droite;
        Droite;Droite;Droite;Droite;Droite;Haut;Haut;Haut;Haut;Gauche;Gauche;Gauche;Gauche;
        Gauche|] ;

      [|Haut;Haut;Haut;Haut;Gauche;Gauche;Gauche;Gauche;Gauche;Bas;Bas;Bas;Gauche;Gauche;
        Gauche;Bas;Bas;Bas;Bas;Bas;Bas|] ; 

      [|Haut;Haut;Haut;Haut;Droite;Droite;Droite;Bas;Bas;Bas;Droite;Droite;Droite;Bas;Bas;
        Bas;Bas;Bas;Bas|] 
    |] (* mouvements predefini *)


let init_monstre i x_m y_m etat_monstre bascule_jeu=
  x_m.(i) <- fst(pos_ghost_init.(i))*20;
  y_m.(i) <- snd(pos_ghost_init.(i))*20;
  etat_monstre.(i) <- En_init;
  num_def.(i) <- 0;
  bascule_jeu.(i) <- Normal;;

let init_monstres x_m y_m etat_monstre bascule_jeu= 
  for i=0 to 3 do
    init_monstre i x_m y_m etat_monstre bascule_jeu;
  done;;

let direction_ideale dx dy i etat_monstre=
  let etat = etat_monstre.(i) in
  if abs(dx) > abs(dy) then
    if dx>0 then if etat=Chasse then Gauche else Droite
    else if etat=Chasse then Droite else Gauche
  else
    if dy>0 then if etat=Chasse then Bas else Haut
    else  if etat=Chasse then Haut else Bas ;;

let newpos i x_pac y_pac x_m y_m etat_monstre direction_courante bascule_jeu= 
  let x = x_m.(i)
  and y = y_m.(i) in
  let dx = x_pac - x in
  let dy = y_pac - y in
  let d = direction_ideale dx dy i etat_monstre in
  
  
    (* 0:Gauche; 1:Droite; 2:Haut; 3:Bas *)
  let xx = x/20 +1 in
  let yy = y/20 in 
  let possibilite = [|(terrain.(yy).((x-20)/20 +1) <> Mur);
                      (terrain.(yy).((x+20)/20 +1) <> Mur);
                      (terrain.((y+20)/20).(xx) <> Mur);
                      (let v=terrain.((y-20)/20).(xx)in (v<>Mur)&&(v<>Sortie));false|] in
  
  if !etat_jeu=bascule_jeu.(i) then
    begin
        match direction_courante.(i) with
          Gauche -> possibilite.(1) <- false
        | Droite -> possibilite.(0) <- false
        | Haut ->possibilite.(3) <- false
        | Bas ->possibilite.(2) <- false
    end
  else
    bascule_jeu.(i) <- !etat_jeu;
  

    (* solution de rechange *)
    let n = 
      let cpt = ref 0 in 
      for i=0 to 3 do if possibilite.(i) then incr cpt done; !cpt in
    let r = ref (Random.int n) in
    let p = ref 0 in
    while (!r>0) do
      if possibilite.(!p) then decr r;
      incr p;
    done;

    while not possibilite.(!p) do
      incr p
    done;
    
    let rechange = 
      (match !p with
        0 -> Gauche
      | 1 -> Droite
      | 2 -> Haut
      | _ -> Bas) in
   
    match d with
      Gauche -> if possibilite.(0) then d else rechange
    | Droite -> if possibilite.(1) then d else rechange
    | Haut -> if possibilite.(2) then d else rechange
    | Bas -> if possibilite.(3) then d else rechange

let mouvement i x_m y_m etat_monstre direction_courante = 
  let x = x_m.(i)
  and y = y_m.(i) in
    match direction_courante.(i) with
      Gauche -> 
        begin
          match terrain.(y/20).((x-1)/20 +1) with
            Pastille | Dot | Vide -> if x=0 then x_m.(i)<-540  else x_m.(i)<-x-1;
          | Mur  -> () ;
          | Sortie -> if etat_monstre.(i) = En_init then x_m.(i)<-x-1;
        end
    | Droite ->
        begin
          match terrain.(y/20).((x+20)/20 +1) with
            Pastille | Dot | Vide ->if x=540 then x_m.(i)<-0 else x_m.(i)<-x+1;
          | Mur -> ();
          | Sortie ->if etat_monstre.(i) = En_init then x_m.(i)<-x+1;
        end    
    | Haut ->
        begin
          match terrain.((y+20)/20).(x/20 +1) with
            Pastille | Dot | Vide -> y_m.(i)<-y+1;
          | Mur  -> () ; 
          | Sortie -> if etat_monstre.(i) = En_init then y_m.(i)<-y+1
        end
    | Bas ->
        begin
          match terrain.((y-1)/20).(x/20 +1) with
            Pastille | Dot | Vide -> y_m.(i)<-y-1;
          | Mur  -> () ;
          | Sortie -> if etat_monstre.(i) = En_init then y_m.(i)<-y-1;
        end

let bouge_monstres x_pac y_pac x_m y_m etat_monstre direction_courante bascule_jeu=
  for i=0 to 3 do
    let x = x_m.(i) 
    and y = y_m.(i) 
    and num = num_def.(i) in
    if (( ((x+20) mod 20)=0) && (( (y+20) mod 20)=0)) then
      if etat_monstre.(i) = En_init then
        begin
          direction_courante.(i) <- mvt_def.(i).(num);
          num_def.(i) <- num + 1 ;
          if num_def.(i)=max_def.(i) then etat_monstre.(i) <- Normal;
        end
      else
        direction_courante.(i) <- newpos i x_pac y_pac x_m y_m etat_monstre direction_courante bascule_jeu;
    mouvement i x_m y_m etat_monstre direction_courante; 
  done;;




(* ---- Un cycle du jeu ---- *)

let attrape x_pac y_pac score x_m y_m etat_monstre bascule_jeu=
  let b = ref false 
  and cpt = ref 0 in
  for i = 0 to 3 do
    if (x_pac=(x_m.(i)+10)) && (y_pac=(y_m.(i)+10)) then
      begin
        incr cpt;
        if etat_monstre.(i)=Chasse then 
          begin
            score := !score + (!cpt*400);
            init_monstre i x_m y_m etat_monstre bascule_jeu;
          end
        else b := true
      end;
  done;
  !b;;
         
(* initialisation du pacman et des monstres *)
let init_player x_pac y_pac bouche score vie x_m y_m etat_monstre d_pac bascule_jeu=
  dessine_fond();
  let x,y = init_pac x_pac y_pac !score !vie in
  init_monstres x_m y_m etat_monstre bascule_jeu;
  affiche x y bouche x_m y_m etat_monstre d_pac;
  update();
  (x,y);;

(* Initialisation de la partie *)
let init_game x_pac y_pac bouche score vie x_m y_m d_pac bascule_jeu=
  chargement_terrain();
  init_player x_pac y_pac bouche score vie x_m y_m d_pac bascule_jeu;;

(* -------- le jeu -------- *)

let x_pac = ref 0 (* coordonnees du pacman: 14-7 au depart*)
and y_pac = ref 0
and d_pac = ref Gauche  (* direction initiale vers la gauche *)
and dd_pac = ref Gauche


and bouche = ref 0 (* bouche du pacman *)
and score = ref 0  
and vie = ref max_vie (* Nombre de vie *)

and x_m = [|0;0;0;0|] (* positions courantes des monstres *)
and y_m = [|0;0;0;0|]
and etat_monstre = [|En_init;En_init;En_init;En_init|] (* comportement predefini au depart *)
and direction_courante = [|Gauche;Gauche;Gauche;Gauche|]
and bascule_jeu = [|Normal;Normal;Normal;Normal|] (* changement de direction autorise *)

let _ =
  while menu () do
    let x,y=init_game !x_pac !y_pac !bouche score vie x_m y_m etat_monstre !d_pac bascule_jeu in
    x_pac := x; y_pac := y;
    vie := max_vie;
    let restart = ref false in
    while !vie>0  do
      if !gagne then 
        begin 
          chargement_terrain(); 
          let x,y = 
            init_player !x_pac !y_pac !bouche score vie x_m y_m etat_monstre !d_pac bascule_jeu in
          x_pac := x ; y_pac := y;
          gagne := false  
        end;
      if !restart then 
        begin 
          let x,y = 
            init_player !x_pac !y_pac !bouche score vie x_m y_m etat_monstre !d_pac bascule_jeu in
          x_pac := x ; y_pac := y ; 
          restart := false 
        end;
(*      Concur.ThreadUnix.select [] [] [] (!delay);  *)
      
      
      if key_pressed() then dd_pac :=chgtdir !d_pac;
      
      if !etat_jeu=Chasse then 
        if !chrono_chasse>0 then 
          begin
            decr chrono_chasse;
            if !chrono_chasse<300 then 
              if (!chrono_chasse mod 8 = 0) then couleur_monstre := not !couleur_monstre
          end
        else 
          begin
            etat_jeu := Normal;
            for i=0 to 3 do
              if etat_monstre.(i)=Chasse then etat_monstre.(i) <- Normal;
            done;
            couleur_monstre := true;
          end;
        
        efface !x_pac !y_pac x_m y_m;
      bouge_monstres !x_pac !y_pac x_m y_m etat_monstre direction_courante bascule_jeu;
      if attrape !x_pac !y_pac score x_m y_m etat_monstre bascule_jeu then 
        begin decr vie; restart := true end
      else
        begin
          let (x,y,z),t = 
            bouge_pac !x_pac !y_pac !bouche score etat_monstre !d_pac !dd_pac 
          in
          x_pac := x;y_pac := y;bouche :=z; d_pac := t;
          
          if not (!gagne) then
            if attrape !x_pac !y_pac score x_m y_m etat_monstre bascule_jeu
            then begin decr vie; restart := true end
            else
            if !etat_jeu=Chasse then
              begin
                let (x,y,z),t=
                  bouge_pac !x_pac !y_pac !bouche score etat_monstre !d_pac !dd_pac
                in
                x_pac := x;y_pac := y;bouche :=z; d_pac :=t;
                
                if attrape !x_pac !y_pac score x_m y_m etat_monstre bascule_jeu
                then 
                  begin decr vie; restart := true end;
              end;
        end;   
      
      affiche !x_pac !y_pac !bouche x_m y_m etat_monstre !d_pac;
      update();
    done;
    perdu();
  done;
  au_revoir();;










