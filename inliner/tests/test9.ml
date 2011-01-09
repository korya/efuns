(*
The following code could be improved by removal of checkbounds and
loop unrolling:

for i = 0 to n do
  A(i)
done

-->

  r0 = r(i)-r(n)
  if r0 < 0 then goto .Lend
  if r0 land 1 = 0 then goto .L1
  A(r(i))
  r(i) ++
.L1
  if r(i) > r(n) then .goto .Lend
  if r0 land 2 = 0 then goto .L2
  A(r(i))
  A(r(i)+1)
  r(i) += 2
.L2
  if r(i) > r(n) then .goto .Lend
  if r0 land 4 = 0 then goto .L4
  A(r(i))
  A(r(i)+1)
  A(r(i)+2)
  A(r(i)+3)
  r(i) += 4
.L4
  if r(i) > r(n) then .goto .Lend
  if r0 land 4 = 0 then goto .L4
.Lnext
  A(r(i))
  A(r(i)+1)
  A(r(i)+2)
  A(r(i)+3)
  A(r(i)+4)
  A(r(i)+5)
  A(r(i)+6)
  A(r(i)+7)
  r(i) += 8
  if r(i) <= r(n) then .goto .Lnext
.Lend
  
if r0 = 1 then goto .L1
if r0 = 2 then goto .L2
if r0 = 3 then goto .L3
.L3
.L2
A(r(i))
r(i) ++
.L1
A(r(i))
r(i) ++
.L0
A(r(i))
r(i) ++
if (r(i) = r(n)) goto .Lend
.Lbegin
A(r(i))
A(r(i)+1)
A(r(i)+2)
A(r(i)+3)
r(i) = r(i) + 4
if (r(i) = r(n)) goto .begin
.Lend  


*)

let sum r =
  let s = ref 0 in
  for i = 0 to Array.length r - 1 do
    s := !s + r.(i)
  done;
  !s

  (*
Test9_sum_39:
.L102:
        movl    %eax, %edx
        movl    $1, %eax
        movl    $1, %ebx
        movl    -4(%edx), %edi # %ecx = lenght(%edx, addr)
        shrl    $9, %edi       # suite
        movl    %edi, %ecx
        orl     $1, %ecx
        addl    $-2, %ecx
        cmpl    %ecx, %ebx    # (h>>9 - 2) | 1 >= %ebx
        jg      .L100
.L101:
        cmpl    %ebx, %edi    # (h >> 9) > %ebx
        jbe     .L103
        movl    -2(%edx, %ebx, 2), %esi
        lea     -1(%eax, %esi), %eax
        addl    $2, %ebx
        cmpl    %ecx, %ebx
        jle     .L101
.L100:
        ret
.L103:
        call    caml_array_bound_error
        .text
*)