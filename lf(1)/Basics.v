(* ================================================================= *)
(** ** Booleans *)

(** In a similar way, we can define the standard type [bool] of
    booleans, with members [true] and [false]. *)

Inductive bool : Type :=
  | true
  | false.

(** Although we are rolling our own booleans here for the sake
    of building up everything from scratch, Coq does, of course,
    provide a default implementation of the booleans, together with a
    multitude of useful functions and lemmas.  (Take a look at
    [Coq.Init.Datatypes] in the Coq library documentation if you're
    interested.)  Whenever possible, we'll name our own definitions
    and theorems so that they exactly coincide with the ones in the
    standard library.

    Functions over booleans can be defined in the same way as
    above: *)

Definition negb (b:bool) : bool :=
  match b with
  | true => false
  | false => true
  end.

Definition andb (b1:bool) (b2:bool) : bool :=
  match b1 with
  | true => b2
  | false => false
  end.

Definition orb (b1:bool) (b2:bool) : bool :=
  match b1 with
  | true => true
  | false => b2
  end.

(** The last two of these illustrate Coq's syntax for
    multi-argument function definitions.  The corresponding
    multi-argument application syntax is illustrated by the following
    "unit tests," which constitute a complete specification -- a truth
    table -- for the [orb] function: *)

Example test_orb1:  (orb true  false) = true.
Proof. simpl. reflexivity.  Qed.
Example test_orb2:  (orb false false) = false.
Proof. simpl. reflexivity.  Qed.
Example test_orb3:  (orb false true)  = true.
Proof. simpl. reflexivity.  Qed.
Example test_orb4:  (orb true  true)  = true.
Proof. simpl. reflexivity.  Qed.

(** We can also introduce some familiar syntax for the boolean
    operations we have just defined. The [Notation] command defines a new
    symbolic notation for an existing definition. *)

Notation "x && y" := (andb x y).
Notation "x || y" := (orb x y).

Example test_orb5:  false || false || true = true.
Proof. simpl. reflexivity. Qed.

(** _A note on notation_: In [.v] files, we use square brackets
    to delimit fragments of Coq code within comments; this convention,
    also used by the [coqdoc] documentation tool, keeps them visually
    separate from the surrounding text.  In the HTML version of the
    files, these pieces of text appear in a [different font].

    The command [Admitted] can be used as a placeholder for an
    incomplete proof.  We'll use it in exercises, to indicate the
    parts that we're leaving for you -- i.e., your job is to replace
    [Admitted]s with real proofs. *)

(** **** Exercise: 1 star, standard (nandb)  

    Remove "[Admitted.]" and complete the definition of the following
    function; then make sure that the [Example] assertions below can
    each be verified by Coq.  (I.e., fill in each proof, following the
    model of the [orb] tests above.) The function should return [true]
    if either or both of its inputs are [false]. *)

Definition nandb (b1:bool) (b2:bool) : bool :=
  match b1 with
  | false => true
  | true => (negb b2)
  end.
  

Example test_nandb1:               (nandb true false) = true.
Proof. simpl. reflexivity.  Qed.

Print test_nandb1.