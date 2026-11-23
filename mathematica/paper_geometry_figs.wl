(* Paper-style versions of the geometry figures (PRD, one column = 3.4 in = 245 pt).
   Run each block in the notebook where the original code already worked (AmpDB, AmpDBPbbsq, DistinctLinkPaths, BIDATEXReader ... must exist).
   Nothing from the original computations is changed, only the Graphics options, labels, colours and the export path.
   Output: figures/panels/geom/*.pdf, each exactly 245 x 183.6 pt like every Python panel. *)

(* ---------- shared style: run this block first ---------- *)
outDir = "/Users/hariprashadravikumar/Lattice_QCD_TMD_PhD/Lattice_QCD_TMD_Sivers_paper/figures/panels/geom/";
If[! DirectoryQ[outDir], CreateDirectory[outDir]];
pw = 245.; ph = 183.6;                                   (* 3.4 x 2.55 in *)
fam = "STIXGeneral";
cBlue = RGBColor["#0072B2"]; cVerm = RGBColor["#D55E00"]; cGreen = RGBColor["#009E73"];
cOrange = RGBColor["#E69F00"]; cPink = RGBColor["#CC79A7"];
plCol = {cBlue, cVerm, cGreen, cOrange};                 (* P_L = -1,-2,-3,-4, same as the Python figures *)
it[s_] := Style[s, Italic];
lab[s_] := Style[s, FontFamily -> fam, FontSize -> 9];
tk[r_, step_] := Table[{k, If[Mod[k, step] == 0, k, ""], {0.012, 0}}, {k, r[[1]], r[[2]]}];
tkn[r_] := Table[{k, "", {0.006, 0}}, {k, r[[1]], r[[2]]}];
meanfromJK[JKvalue_] := MakeErrBars[{{1, JKvalue}}][[1]][[1]][[2]];
paperOpts[xr_, yr_, xstep_, ystep_, xl_, yl_] := Sequence[
   Frame -> True, Axes -> False, AspectRatio -> Full,
   ImageSize -> {pw, ph}, ImagePadding -> {{34, 8}, {27, 8}},
   BaseStyle -> {FontFamily -> fam, FontSize -> 8},
   FrameLabel -> {lab[xl], lab[yl]},
   FrameTicks -> {{tk[yr, ystep], tkn[yr]}, {tk[xr, xstep], tkn[xr]}},
   FrameTicksStyle -> Directive[FontFamily -> fam, FontSize -> 8],
   GridLines -> {Range[xr[[1]], xr[[2]]], Range[yr[[1]], yr[[2]]]},
   GridLinesStyle -> Directive[GrayLevel[0.85], AbsoluteThickness[0.4]],
   PlotRange -> {xr, yr}, PlotRangePadding -> 0];

(* ---------- 1. bL_bT_plot.pdf ---------- *)
bLbTlist = MemDBGet[AmpDB, MemDBSelect[AmpDB, {}], {bL, bT}] // Union;
fig1 = Graphics[{
    {cVerm, AbsoluteThickness[0.8], Line[{{0, 0}, #}] & /@ bLbTlist},
    {cVerm, PointSize[0.014], Point[bLbTlist]},
    {Black, PointSize[0.02], Point[{0, 0}]}},
   paperOpts[{-9, 9}, {-8, 8}, 2, 2,
    Row[{Subscript[it["b"], 1], "/", it["a"]}], Row[{Subscript[it["b"], 2], "/", it["a"]}]]];
Export[outDir <> "bL_bT_plot.pdf", fig1]

(* ---------- 2. bL_bT_plot_with_path.pdf (needs DistinctLinkPaths and PathToLinks from your notebook, the 6-direction version) ---------- *)
PathToLinks6[sidelink_] := Module[{u = {{0, 0, 0}}, d},
   Do[d = Switch[sidelink[[i]], 1, {1, 0, 0}, -1, {-1, 0, 0}, 2, {0, 1, 0}, -2, {0, -1, 0}, 3, {0, 0, 1}, -3, {0, 0, -1}, _, {0, 0, 0}];
    AppendTo[u, u[[-1]] + d], {i, Length[sidelink]}]; u];
paths2D = Map[#[[1 ;; 2]] &, Table[PathToLinks6[DistinctLinkPaths[[i]]], {i, Length[DistinctLinkPaths]}], {2}];
fig2 = Graphics[{
    {cBlue, Dashed, AbsoluteThickness[0.8], Opacity[0.6], Line[paths2D]},
    {cVerm, AbsoluteThickness[0.8], Line[{{0, 0}, #}] & /@ bLbTlist},
    {cVerm, PointSize[0.014], Point[bLbTlist]},
    {Black, PointSize[0.02], Point[{0, 0}]}},
   paperOpts[{-9, 9}, {-8, 8}, 2, 2,
    Row[{Subscript[it["b"], 1], "/", it["a"]}], Row[{Subscript[it["b"], 2], "/", it["a"]}]]];
Export[outDir <> "bL_bT_plot_with_path.pdf", fig2]

(* ---------- 3. Pb_bsq_plot.pdf (the data is not shown in the paper any more as (P.b, b^2) space; run only if you still want the file) ---------- *)
plValues = {-1, -2, -3, -4}; zetaValues = {0.090772, 0.294367, 0.539684, 0.785756};
markers = {"\[FilledDiamond]", "\[FivePointedStar]", "\[FilledUpTriangle]", "\[FilledSquare]"};
prim = Table[Module[{l, bl},
    l = MemDBGet[AmpDBPbbsq, MemDBSelect[AmpDBPbbsq, {PL -> plValues[[i]]}], {Pb32by2Pi, bsquare}] // Union;
    bl = l /. {p_, b2_} :> {Sqrt[b2], p};
    {plCol[[i]], Text[Style[markers[[i]], 7], #] & /@ bl}], {i, 4}];
leg = Table[{plCol[[i]], Text[Style[markers[[i]], 7], {0.55, 31 - 5 i}],
    Black, Text[Style[Row[{Overscript["\[Zeta]", "^"], "=", zetaValues[[i]]}], 8, FontFamily -> fam], {0.9, 31 - 5 i}, {-1, 0}]}, {i, 4}];
fig3 = Graphics[{prim, leg},
   paperOpts[{0, 9}, {-35, 35}, 1, 10, Row[{"\!\(\*SqrtBox[SuperscriptBox[\"b\", \"2\"]]\)", "/", it["a"]}],
    Row[{"(", it["P"], "\[CenterDot]", it["b"], ")\[CenterDot]", OverscriptBox[it["L"], "^"], "/(2\[Pi])"}]]];
Export[outDir <> "Pb_bsq_plot.pdf", fig3]

(* ---------- 4. v1_v2_allzetahat_plot.pdf ---------- *)
etavalue = 10; dbNoList = {1, 2, 3, 4}; els = {}; entries = {};
Do[dbNo = dbNoList[[k]]; c = plCol[[k]];
  NEdb = BIDATEXReader["/Users/hariprashadravikumar/sivers_TMD_PhD_project/sivers_new/stripped_" <> ToString[dbNo] <> "/NucleonEnergy_jn.bidatex"];
  P0 = MemDBGet[NEdb, MemDBSelect[NEdb, {}], energy][[1]]; II = P0/P0;
  P1 = dbNo*II*((2 Pi)/32); Msquare = P0*P0 - P1*P1; M = Sqrt[Msquare];
  ztahat = Sqrt[(P1*P1)/(M*M*(((M*M)/(P1*P1)) + 2 II))];
  v1byv3jakk = (M*ztahat)/Sqrt[(P1*P1) - M*M*ztahat*ztahat];
  v1BYv3 = Abs[MakeErrBars[{{1, v1byv3jakk}}][[1]][[1]][[2]]];
  vv = Table[{v3*v1BYv3, v3}, {v3, 0, etavalue}];
  AppendTo[els, {c, AbsoluteThickness[1.2], Line[vv], PointSize[0.012], Point[vv]}];
  AppendTo[entries, {c, Round[meanfromJK[ztahat], 0.001], Round[v1BYv3, 0.001]}], {k, 4}];
leg4 = Table[{entries[[i, 1]], AbsoluteThickness[1.2], Line[{{0.4, 9.4 - 0.9 i}, {1.4, 9.4 - 0.9 i}}],
    Black, Text[Style[Row[{Overscript["\[Zeta]", "^"], "=", entries[[i, 2]], ",  ", Subscript[it["v"], 1], "=", entries[[i, 3]], Subscript[it["v"], 3]}], 8, FontFamily -> fam], {1.7, 9.4 - 0.9 i}, {-1, 0}]}, {i, 4}];
fig4 = Graphics[{els, leg4},
   paperOpts[{0, 10}, {0, 10}, 2, 2, Row[{Subscript[it["v"], 1], "/", it["a"]}], Row[{Subscript[it["v"], 3], "/", it["a"]}]]];
Export[outDir <> "v1_v2_allzetahat_plot.pdf", fig4]

(* ---------- 5. v1_v2_withPath_forP-1..4 (one figure per momentum; the gauge-link tables are not re-exported, they are LaTeX tables in the paper) ---------- *)
etaValues = Range[10, 0, -1]; maxEta = Max[etaValues];
PathToLinks13[sidelink_] := Module[{u = {{0, 0, 0}}, d},
   Do[d = Switch[sidelink[[i]], 1, {1, 0, 0}, 3, {0, 0, 1}, _, {0, 0, 0}]; AppendTo[u, u[[-1]] + d], {i, Length[sidelink]}]; u];
Do[
  platdb = BIDATEXReader["/Users/hariprashadravikumar/sivers_TMD_PhD_project/sivers_new/stripped_" <> ToString[dbNo] <> "/UminusD_Plateaus_jn.bidatex"];
  NEdb = BIDATEXReader["/Users/hariprashadravikumar/sivers_TMD_PhD_project/sivers_new/stripped_" <> ToString[dbNo] <> "/NucleonEnergy_jn.bidatex"];
  P0 = MemDBGet[NEdb, MemDBSelect[NEdb, {}], energy][[1]]; II = P0/P0;
  P1 = dbNo*II*((2 Pi)/32); Msquare = P0*P0 - P1*P1; M = Sqrt[Msquare];
  ztahat = Sqrt[(P1*P1)/(M*M*(((M*M)/(P1*P1)) + 2 II))];
  v1byv3jakk = (M*ztahat)/Sqrt[(P1*P1) - M*M*ztahat*ztahat];
  v1BYv3 = Abs[MakeErrBars[{{1, v1byv3jakk}}][[1]][[1]][[2]]];
  vvector = Table[{v3*v1BYv3, v3}, {v3, 0, maxEta}];
  vlinksall = Union[MemDBGet[platdb, MemDBSelect[platdb, {numberFilter -> Re}], sideLink]];
  vlinks = Complement[vlinksall, Table[ConstantArray[1, i], {i, 1, 14}]];
  lat = Table[Module[{vl, pts},
      vl = Select[vlinks, Count[#, 3] == eta &];
      pts = PathToLinks13 /@ vl;
      Table[{ColorData["Viridis"][i/Max[Length[pts], 1]], AbsoluteThickness[0.9],
        Dashing[Switch[Mod[i, 4, 1], 1, {0.04, 0.02}, 2, {0.015, 0.015}, 3, {0.04, 0.02, 0.005, 0.02}, 4, {0.04, 0.015, 0.005, 0.015}]],
        Line[pts[[i, All, {1, 3}]]], PointSize[0.010], Point[Last[pts[[i, All, {1, 3}]]]]}, {i, Length[pts]}]], {eta, etaValues}];
  legP = {Black, Text[Style[Row[{Overscript["\[Zeta]", "^"], "=", Round[meanfromJK[ztahat], 0.001]}], 8, FontFamily -> fam], {5.6, 2.4}, {-1, 0}],
    cBlue, AbsoluteThickness[1.4], Line[{{4.2, 1.5}, {5.2, 1.5}}], Black,
    Text[Style[Row[{Subscript[it["v"], 1], "=", Subscript[it["v"], 3], "\[Times]", Round[v1BYv3, 0.001]}], 8, FontFamily -> fam], {5.6, 1.5}, {-1, 0}],
    cBlue, PointSize[0.012], Point[{4.7, 0.6}], Black, Text[Style[it["\[Eta]"], 8, FontFamily -> fam], {5.6, 0.6}, {-1, 0}]};
  fig5 = Graphics[{lat, cBlue, PointSize[0.012], Point[vvector], AbsoluteThickness[1.4], Line[vvector], legP},
    paperOpts[{0, 10}, {0, 10}, 2, 2, Row[{Subscript[it["v"], 1], "/", it["a"]}], Row[{Subscript[it["v"], 3], "/", it["a"]}]]];
  Export[outDir <> "v1_v2_withPath_forP-" <> ToString[dbNo] <> "_plot.pdf", fig5],
  {dbNo, 1, 4}]
