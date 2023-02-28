Theorem t_12: forall P1 P2 : Prop, P1 -> (P1 -> P2)-> P2.
Proof.
intros P1 P2 H1 H2.
apply H2.
apply H1.
Qed.

Theorem t_13: forall S : Prop, ~ ~(S \/ ~S).
Proof.
unfold not.
intros S H1.
apply H1.
right.
intro H2.
apply H1.
left.
apply H2.
Qed.

Theorem t_14: forall S : Prop, ~ ~ ~ S -> ~ S.
Proof.
unfold not.
intros S H1 H2.
apply H1.
intro H3.
apply H3.
apply H2.
Qed.

Theorem t_16: forall P : Prop, ~ ~(~ ~ P -> P).
Proof.
unfold not.
intros P H1.
apply H1.
intro H2.
elim H2.
intro H3.
apply H1.
intro H4.
apply H3.
Qed.

Theorem t_19: forall P Q: Prop, ((~P->~Q)->(~P->Q)->P)->~~P->P.
Proof.
unfold not.
intros P Q H1 H2.
apply H1.
intros H3 H4.
apply H3.
elim H2.
intro H5.
apply H3.
apply H5.
intro H3.
elim H2.
apply H3.
Qed.

(* Alternative approach *)
Theorem t_18_alt: forall P: Prop, ((~P->P)->P)->~~P->P.
Proof.
unfold not.
intros P H1 H2.
apply H1.
intro H3.
elim H2.
apply H3.
Qed.
