(***********************************************************************)
(*                                                                     *)
(*                            GwML                                     *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Themes
open Options
open Gwml_args
open Xtypes
open Gwml
open Stdconfig
open AppMgr
open Stddeco
open Wob
open Stdgwmlrc
      
let _ = 
  add_to_path graphics_path "/usr/share/icons";
  add_to_path graphics_path "/usr/share/icons/mini";
  Printf.printf "Gwmlrc loaded"; print_newline ();
  (**********************************
  The following command MUST be executed in your gwmlrc, so that the
  default configuration can be extended. Otherwise, some basic 
  functionalities might not be available, and windows might not be
  decorated at all.
  ************************************)
  Stdgwmlrc.init ()

  
  (************
  Modify "comp_name" to shorten title of windows.
  *************)
  
let regexp = ".pa.dec.com"
let regexp_len = String.length regexp
  
let streqn str1 pos str2 =
  let l1 = String.length str1 in
  let l2 = String.length str2 in
  if pos + l2 > l1 then false else
  try
    for j = 0 to l2 - 1 do
      if str1.[pos+j] <> str2.[j] then raise Exit
    done;
    true
  with
    Exit -> false
      
let strend str pos =
  String.sub str pos (String.length str - pos)

let replace regexp repl s =
  let regexp_len = String.length regexp in
  let repl_len = String.length repl in
  let max = String.length s - regexp_len in
  let rec iter pos max name =
    if pos > max then name else
    if streqn name pos regexp then 
      iter pos (max - regexp_len + repl_len) ((String.sub name 0 pos)^ repl ^
          (strend name (pos+regexp_len)))
    else
      iter (pos+1) max name
  in
  iter 0 max s

let rloginu station login w =
  let _ = Sys.command (Printf.sprintf "(xterm -e rlogin -l %s %s)&" login station) in ()
  
  
let _ =
  comp_name := (fun name ->
  (* shorten hostnames *)
      (replace ".pa.dec.com" ""
          (replace "specialix" "dell" 
            (replace ".inria.fr" ""
              (replace "virtualc5" "v5"           
                (* shorten specific dirs *)
                (replace "~/devel/petal/" "PETAL:"
                  (replace "~/devel/efuns" "EFUNS"
                    (replace "~/devel/jocaml" "JOCAML:"
                    
                    (* shorten home dirs *)
                      (replace "/home/talbot/lefessan" "(T)~" 
                        (replace "/usr/home/lefessan" "(M)~" 
                          (replace "/home/specialix/lefessan" "(H)~" 
                            name))))))))))
  )

  (***********
  From this list of machines, I create a menu, with sub-menus for each
  project.
  ************)
  
let _ =
  hosts := [ 
    "Serveurs", [
      "beaune"; "brouilly"; "cornas"; "margaux"; "passoire";
      "pauillac"; "tif"; "yquem"];
    "Para", [
      "amboise"; "azay"; "babyduck"; "bellet"; "beaune"; "chablis"; "cheilly";
      "chianti"; "chignin"; "clicquot"; "conti"; "couchey"; "emilion";
      "givry"; "kpriss"; "latache"; "layon"; "listel"; "macocotte";
      "macopine"; "macon"; "romanee"; "saumur"; "specialix"; "syrah";
      "talbot"; "toul"; "volnay"
    ];
    "Cristal", [
      "akebono"; "amrita"; "aspirateur"; "ausone"; "banyuls"; "bergerac";
      "bertagna"; "bouzy"; "cadet"; "cadillac"; "cahors"; "corton"; 
      "ephemere"; "figeac"; "foch"; "fronsac"; "jaune"; "jurancon"; "latour";
      "leognan"; "leoville"; "loupiac"; "madiran"; "morgon"; "mouton"; 
      "pauillac"; "pavie"; "peray"; "pernand"; "pessac"; "petrus"; 
      "pomerol"; "pouilly"; "olive";
      "quincy"; "reuilly"; "riesling"; "rieussec"; "santenay"; "vougeot";
    ];
    "Sor",
    [
      "alix"; "aria"; "atchoum"; "azrael"; "blueberry"; "carmen"; "corto"; 
      "dormeur"; "druuna"; "epaf"; "felina"; "goudurix"; "grincheux";
      "incal"; "iznogoud"; "joyeux"; "olrik"; "prof"; "rahan"; "simplet";
      "spip"; "timide"; "tif"; "xiii"; "xombul"; "yoko"; "zorglub"
    ];
    "Algo", [
      "amour"; "bandol"; "blagny"; "charmes"; "chinon"; "chorey"; "crepy";
      "fleurie"; "hermitage"; "lirac"; "menetou"; "meursault"; "montagny"; 
      "monthelie"; "musigny"; "nuits"; "quarts"; "roman"; "rully"; "tokay";
      "tricastin"; "vertigo"; "alfleila"; "pommard"];
    "Cedia", [    
      "ayaam"; "chenas"; "cramant"; "gaillac"; "gentiane"; "julienas"; 
      "kiravi"; "klevner"; "layon"; "mercurey"; "morey"; "rasteau"; "regnie";
      "tavel"; "cassis"];
    "Croap", [
      "arbin"; "barsac"; "cognac"; "condrieu"; "fixin"; "gevrey"; "graves"; 
      "huaine"; "listrac"; "marix"; "medoc"; "moulis"; "muscat"; "pinot";
      "pogo"; "rilly"; "sancerre"; "spiegel"; "sylvaner"; "tobago"; "wallis"; 
      "montlouis"; "calva"; "chouchen"; "marc"; "lussac"; "fuisse"; 
      "moscato"; "kokoro"; "keywest"
      ];
    "Icsla", [
      "abymes"; "arbois"; "auxey"; "blaye"; "lalande"; "paille"; "savigny"
      ];    
      "Loco", ["acteon"; "barr"; "bearn"; "borba"; "ladoix"; "mikonos"; 
      "saering"; "loews"
      ];
    "Espion", ["prisse"];
    "Langage", [
      "athos"; "ariane"; "phedre"; "astree"; "minos"; "thesee"; "cerbere";
      "egee"; "ondine"; "skyros"; "xios"; "ionie"; "ikaria"; "vodka";
      "whisky"; "armagnac"; "chacom"; "kea"; "ouzo"; "sake"; "samos";
      "tinos"; "palinka"; "pandora"; "cronos"; "alfleila"; "ajax";
      "hector"; "cardhu"
    ];
      "Verso", [
        "calendos"; "beaugos"; "salgos"; "gringos"; "logos"; "cosmos"; 
        "vamos"; "craignos"; "lebos"; "pathos"; "lotos"; "chaos"; "tacos";
        "carabos"; "chronos"; "albatros"; "calvados"
      ];
  ];
