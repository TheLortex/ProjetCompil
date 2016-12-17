
(* Bibliothèque pour produire du code assembleur X86-64

   2015 Jean-Christophe Filliâtre (CNRS)
        Kim Nguyen (Université Paris Sud)
*)

(** {0 Bibliothèque pour l'écriture de programmes X86-64 }

    Il s'agit là uniquement d'un fragment relativement petit
    de l'assembleur X86-64. *)

(** Le module {!X86_64} permet l'écriture de code X86-64 dans du code
    OCaml, sans utiliser un préprocesseur.  Un exemple complet est
    donné {{:#1_Exemple}ci-dessous, dans la section exemple}. *)

type 'a asm
  (** type abstrait pour représenter du code assembleur. Le paramètre
      ['a] est utilisé comme type fantôme. *)

type text = [ `text ] asm
  (** type représentant du code assembleur se trouvant dans la zone de
      texte *)

type data = [ `data ] asm
  (** type représentant du code assembleur se trouvant dans la zone de
      données *)

val nop : [> ] asm
  (** l'instruction vide. Peut se trouver dans du text ou du data *)

val ( ++ ) : ([< `text|`data ] asm as 'a)-> 'a -> 'a
  (** concatène deux bouts de codes (soit text avec text, soit data avec
      data) *)

type program = {
  text : text;
  data : data;
}
  (** un programme est constitué d'une zone de texte et d'une zone de données *)

val print_program : Format.formatter -> program -> unit
  (** [print_program fmt p] imprime le code du programme [p] dans le
      formatter [fmt] *)

val print_in_file: file:string -> program -> unit

type register
  (** Type abstrait pour les registres *)

val rax: register
val rbx: register
val rcx: register
val rdx: register
val rsi: register
val rdi: register
val rbp: register
val rsp: register
val r8 : register
val r9 : register
val r10: register
val r11: register
val r12: register
val r13: register
val r14: register
val r15: register
  (** Constantes représentant les registres manipulables. *)


type label = string
  (** Les étiquettes d'addresses sont des chaînes de caractères *)

type operand
  (** Le type abstrait des opérandes *)

val imm: int -> operand
  (** $i *)
val reg: register -> operand
val ind: ?ofs:int -> ?index:register -> ?scale:int -> register -> operand
  (** ofs(register, index, scale) *)
val lab: label -> operand
  (** L  *)
val ilab: label -> operand
  (** $L *)

(** {1 Move} *)

val movq: operand -> operand -> text
  (** attention : toutes les combinaisons d'opérandes ne sont pas permises *)

(** {1 Opérations arithmétiques } *)

val leaq: operand -> register -> text

val addq: operand -> operand -> text
val subq: operand -> operand -> text
val imulq: operand -> operand -> text
val idivq: operand -> text

val negq: operand -> text

(** {1 Opérations logiques } *)

val andq: operand -> operand -> text
val orq : operand -> operand -> text
val xorq: operand -> operand -> text
val notq: operand -> text
  (** Opérations de manipulation de bits. "et" bit à bit, "ou" bit à
       bit, "not" bit à bit *)

(** {1 Sauts } *)

val jmp : label -> text
  (** saut inconditionnel *)

val call: label -> text
val ret: text
  (** appel de fonction et retour *)

(** {1 Divers } *)

val label : label -> [> ] asm
  (** un label. Peut se retrouver dans du text ou du data *)
val glabel : label -> [> ] asm
  (** même chose, avec une déclaration .globl (pour main, typiquement) *)

val comment : string -> [> ] asm
  (** place un commentaire dans le code généré. Peut se retrouver dans
      du text ou du data *)

val string : string -> data
  (** place une constante chaîne de carctères (terminées par 0) dans a
      zone data *)
val dbyte : int list -> data
val dword : int list -> data
val dint : int list -> data
val dquad : int list -> data
  (** place une liste de valeurs sur 1/2/4/8 octets dans la zone data *)

val space: int -> data
  (** [space n] alloue [n] octets (valant 0) dans le segment de données *)

val inline: string -> [> ] asm
  (** [inline s] recopie la chaîne [s] telle quelle dans le fichier
      assembleur *)

(** {1 Manipulation de la pile} *)

val pushq : register -> text
  (** [pushq r] place le contenu de [r] au sommet de la pile.
      Rappel : %rsp pointe sur l'adresse de la dernière case occupée *)

val popq : register -> text
  (** [popq r] place le mot en sommet de pile dans [r] et dépile *)

