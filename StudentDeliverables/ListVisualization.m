(* ::Package:: *)

BeginPackage["ListVisualization`"]

(* Define VisualizeReverse function *)
VisualizeReverse[list_]:= Module[{},

(* Color Association of assigning the colors to elements of the list *)
colorAssociation=<|1->Red,2->Blue,3->Green,4-> Yellow,5->Purple,6-> Pink,7-> Orange,8 -> RGBColor[0.9,0.5,0.1],9-> RGBColor[0.23,0.95,0.8], 10-> Brown|>;

(* Assign colors to the elements of the list *)
listNew =Table[Style[list[[i]],FontColor->colorAssociation[i]],{i,Length[list]}];

(* Convert Input and output into image *)
img1 = Rasterize[Column[{StringJoin[{"Reverse[",ToString[listNew,StandardForm],"]"}]," "," "," ",Row[{" "ToString[Reverse[listNew],StandardForm]}]}],ImageSize->500];

(* Input form for recognising the Color of the elements *)
aaa = InputForm[listNew];

(* Finding the postion for particular color *)
imglist = Table[ImageValuePositions[img1,aaa[[1,k,2,2]],0.05],{k,Length[list]}];

(* Draw the Arrows using Graphics command *)
RevImage = ImageCompose[img1,Show[Table[Graphics[{Arrowheads[0.03],Arrow[{imglist[[i,1]],imglist[[i,Ceiling[Length[imglist[[i]]]/2 + 1]]]}]}],{i,Length[list]}]]]
]

(* Define VisualizeSort function *)
VisualizeSort[list_]:= Module[{},

(* Color Association of assigning the colors to elements of the list *)
colorAssociation=<|1->Red,2->Blue,3->Green,4-> Yellow,5->Purple,6-> Pink,7-> Orange,8 -> RGBColor[0.9,0.5,0.1],9-> RGBColor[0.23,0.95,0.8], 10-> Brown|>;

(* Assign colors to the elements of the list *)
listNew =Table[Style[list[[i]],FontColor->colorAssociation[i]],{i,Length[list]}];

(* Convert Input and output into image *)
img1 = Rasterize[Column[{StringJoin[{"Sort[",ToString[listNew,StandardForm],"]"}]," "," "," ",Row[{" ",ToString[Sort[listNew],StandardForm]}]}],ImageSize->500];

(* Input form for recognising the Color of the elements *)
aaa = InputForm[listNew];

(* Finding the postion for particular color *)
imglist = Table[ImageValuePositions[img1,aaa[[1,k,2,2]],0.05],{k,Length[list]}];

(* Draw the Arrows using Graphics command *)
SortImage = ImageCompose[img1,Show[Table[Graphics[{Arrowheads[0.02],Arrow[{imglist[[i,1]],imglist[[i,Ceiling[Length[imglist[[i]]]/2 + 1]]]}]}],{i,Length[list]}]]]
]


(* Define VisualizeRiffle function *)
VisualizeRiffle[list_,x_]:= Module[{},

(* Assign Black color to the elements of the list *)
listNew =Table[Style[list[[i]],FontColor->Black],{i,Length[list]}];

(* Convert Input and output into image with giving blue color to outer element *)
img1 = Rasterize[Column[{StringJoin[{"Riffle[",ToString[list,StandardForm],",",ToString[Style[x,FontColor->Blue],StandardForm],"]"}]," "," "," ",Row[{ToString[Riffle[listNew,Style[x,FontColor->Blue]],StandardForm]}]}],ImageSize->500];

(* Finding the postion for particular color *)
imglist = ImageValuePositions[img1,Blue];

(* Sorting the list which contains image positions of output image *)
newimglist = Sort[Drop[imglist,Length[imglist]/Length[list]]];

(* Draw the Arrows using Graphics command *)
RiffleImage = ImageCompose[img1,Show[Table[Graphics[{Arrowheads[0.03],Arrow[{imglist[[Ceiling[Length[imglist]/(2*Length[list])]]],newimglist[[(i*Length[newimglist]/(Length[list]-1))]]-{0,10}}]}],{i,Length[list]-1}]]]
]

(* Define VisualizeTranspose function *)
VisualizeTranspose[list_]:= Module[{},

(* Convert Partition of the list to a single list *)
listNew = Flatten[list];

(* Color Association of assigning the colors to elements of the list *)
colorAssociation=<|1->Red,2->Blue,3->Green,4-> Yellow,5->Purple,6-> Pink,7-> Orange,8 -> RGBColor[0.9,0.5,0.1],9-> RGBColor[0.23,0.95,0.8], 10-> Brown|>;

(* Assign colors to the elements of the list *)
listNew = Table[Style[listNew[[i]],FontColor->colorAssociation[i]],{i,Length[listNew]}];

(* Convert the single list into partition *)
listNew = Partition[listNew,Length[list[[1]]]];

(* Convert Input and output into image *)
img1 = Rasterize[Column[{StringJoin[{"Transpose[",ToString[listNew,StandardForm],"]"}]," "," "," ",Row[{" "ToString[Transpose[listNew],StandardForm]}]}],ImageSize->500];

(* Input form for recognising the Color of the elements *)
aaa = InputForm[Flatten[listNew]];

(* Finding the postion for particular color *)
imglist = Table[ImageValuePositions[img1,aaa[[1,k,2,2]],0.05],{k,Length[Flatten[list]]}];

(* Draw the Arrows using Graphics command *)
TransposeImage = ImageCompose[img1,Show[Table[Graphics[{Arrowheads[0.03],Arrow[{imglist[[i,1]],imglist[[i,Ceiling[Length[imglist[[i]]]/2 + 1]]]}]},ImageSize->30],{i,Length[Flatten[list]]}]]]
]

EndPackage[]


(* ::InheritFromParent:: *)
(*"ListVisualization`"*)
