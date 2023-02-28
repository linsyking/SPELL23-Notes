Theorem t_11: forall P1 P2 : Prop, P1 -> (P1 -> P2)-> P2.
Proof.
intros P1 P2 H1 H2.
apply H2.
apply H1.
Qed.

Theorem t_12: forall S : Prop, ~ ~(S \/ ~S).
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

Theorem t_13: forall S : Prop, ~ ~ ~ S -> ~ S.
Proof.
unfold not.
intros S H1 H2.
apply H1.
intro H3.
apply H3.
apply H2.
Qed.

Theorem t_15: forall P : Prop, ~ ~(~ ~ P -> P).
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