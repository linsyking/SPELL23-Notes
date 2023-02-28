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
