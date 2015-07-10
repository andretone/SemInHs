Linguaggio (con lambda calcolo) EAGER di ordine superiore

semantiche operazionali, denotazionali e calcolo di approssimazini di funzioni.

- LamUntiped.hs : troviamo l'interprete che segue la semantica operazionale, è stato inplementato
anche l'interprete che si occupa del lambda calcolo.

- Denotational.hs : troviamo metodi per calcolare la semantica denotazionale del linguaggio eager.
Sege le definizioni del libro "The Formal Semantics of Programming Languages", Glynn Winskel.

- Approximations.hs : offre dei metodi per calcolare delle approssimazioni del linguaggio.
Le approssimazioni sono rappresentate come liste di coppie. Non sono implementate le coppie come tipo,
e quindi le relative operazioni first e second, ma solo gli interi.

- Syntax.hs : è definita la sintassi del linguaggio

- Nei moduli Testxxxxxx.hs si trovano degli esempi di programmi e invocazioni dei relativi moduli.


- esempi di utilizzo:

--Semantica OPERAZIONALE:

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

Per interpretare un programma si utilizza runEval, consultare TestSemOperazionale.hs
per ulteriori esempi.
Per visualizzare meglio le closure ottenute dall'esecuzione, applicare v2e
che adatta i valori ottenuti a termini.

--SEMANTICA DENOTAZIONALE:
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

denote permette di calcolare seguendo la semantica denotazionale,
consultare TestSemDenotazionale.hs per altri esempi.

--APPROSSIMAZIONI:
attenzione a non generare liste infinite! (usare la funzione take sulle liste per limitare i risultati)
viene fornita anche filter2show, che tronca le liste ai primi n elmti.

..> :l Appproximation.hs

..> :m + Syntax

..>filter2show 8 $ ( denote' (Lit (LInt 4)) emptyEnv )

N 4
