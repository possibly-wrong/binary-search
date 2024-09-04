(* List all possible search strategies with key space[n]. *)
strategies[n_] := strategies[n] = Module[
   {guess},
   If[n === 0, {{}},
    Join @@ Table[Join @@ Outer[
        {guess, #1, #2} &,
        strategies[guess - 1],
        guess + strategies[n - guess],
        1
        ],
      {guess, 1, n}
      ]]]
      
(* Play out a pure strategy game. *)
play[key_, strategy_] := Module[
  {guess, lower, higher},
  {guess, lower, higher} = strategy;
  1 + Which[
    key === guess, 0,
    key < guess, play[key, lower],
    key > guess, play[key, higher]
    ]]

(* Compute Nash equilibrium value and mixed strategy for row player. *)
(* solve[-Transpose[payoff]] solves for the column player. *)
solve[payoff_] := Module[
  {m, n, d, x, v},
  {m, n} = Dimensions[payoff];
  (* Translate payoff matrix so all entries are positive. *)
  d = -Min[payoff] + 1;
  (* Solve transformed problem. *)
  x = LinearProgramming[
    Table[1, {m}],
    Transpose[payoff + d],
    Table[1, {n}]
    ];
  v = 1/Total[x];
  (* Convert solution back to original problem space. *)
  {v - d, v x}
  ]

(* Compute Alice's strategy for tractable range of n. *)
Do[
 payoff = Outer[play, Range[n], strategies[n], 1];
 Print[n -> solve[payoff]],
 {n, 1, 13}
 ]
