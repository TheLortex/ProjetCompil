open X86_64

let rec iter n code =
  if n = 0 then nop else code ++ iter (n - 1) code
