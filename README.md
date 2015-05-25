Linguaggio (con lambda calcolo) EAGER di ordine superiore

semantiche operazionali, denotazionali e calcolo di approssimazini di funzioni.


esempi di utilizzo:

Semantica OPERAZIONALE:

prompt>ghci

Prelude> :l LamUntiped
[...]

LamUntiped Syntax> :m + Syntax
[..]

LamUntiped Syntax> runEval (App (Lam "x" (Syntax.Sum (Var "x") (Lit(LInt 3)))  ) (Lit (LInt 9)))

Loading package transformers-0.2.2.0 ... linking ... done.

Loading package mtl-2.0.1.0 ... linking ... done.

Loading package array-0.4.0.0 ... linking ... done.

Loading package deepseq-1.3.0.0 ... linking ... done.

Loading package containers-0.4.2.1 ... linking ... done.

(12,[(1,Lam "x" (Sum (Var "x") (Lit (LInt 3)))),(1,Lit (LInt 9)),(2,Var "x")])

SEMANTICA DENOTAZIONALE:
caricare moduli

prompt>ghci

Prelude> :l Denotational Syntax

[..]

Prelude Syntax Denotational> :m + Syntax

[..]

Prelude Syntax Denotational> (Lit (LInt 3))

Lit (LInt 3)

Prelude Syntax Denotational> denote (Syntax.Lit (Syntax.LInt 1)) emptyEnv

Loading package array-0.4.0.0 ... linking ... done.

Loading package deepseq-1.3.0.0 ... linking ... done.

Loading package containers-0.4.2.1 ... linking ... done.

Just 1

APPROSSIMAZIONI:
attenzione a non generare liste infinite! (usare la funzione take sulle liste per limitare i risultati)
viene fornita anche filter2show, che tronca le liste ai primi 5 elmti.

..> :l Appproximation.hs

..> :m + Syntax

..>filter2show $ ( approx (Lit (LInt 4)) emptyEnv )

N 4
