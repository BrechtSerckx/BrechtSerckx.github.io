---
date: 2018 - 2019
title: Master's Thesis
subtitle: Under supervision of Prof. Dr. Ir. Tom Schrijvers
---
_Eff_ is an experimental functional programming language that considers computational side effects as algebraic operations or effects and captures them with effect handlers.  
Eff is compiled to OCaml via the internal calculi _ImpEff_ and _ExEff_.
To make the compiled code run as fast as possible, optimizations were made to ExEff.  
This Master's thesis maps the optimizations in ExEff, adds some new optimizations, and proposes the preservation of type for the optimizations.
In addition, the Master's thesis attempts to evaluate the performance of the generated code compared to non-optimized code.
