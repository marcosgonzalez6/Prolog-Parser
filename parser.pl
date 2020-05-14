:- use_rendering(svgtree, [list(false)]).
:- use_module(library(tabling)).
:- table np/4, dp/4, vp/4, pp/4, adjp/4, advp/4, ip/4.
:- discontiguous v0/4.

%------Reglas lexicas------

% nombre Features = [[rasgos phi],[rasgos semánticos]]
%   NOMBRES COMUNES
n0(n0(linguista), [[_,_,s],[com,anim]]) --> [linguista].
n0(n0(linguistas), [[_,_,p],[com,anim]]) --> [linguistas].
n0(n0(investigador), [[m,_,s],[com,anim]]) --> [investigador].
n0(n0(investigadores), [[m,_,p],[com,anim]]) --> [investigadores].
n0(n0(investigadora), [[f,_,s],[com,anim]]) --> [investigadora].
n0(n0(investigadoras), [[f,_,p],[com,anim]]) --> [investigadoras].
n0(n0(ingeniero), [[m,_,s],[com,anim]]) --> [ingeniero].
n0(n0(ingenieros), [[m,_,p],[com,anim]]) --> [ingenieros].
n0(n0(ingeniera), [[f,_,s],[com,anim]]) --> [ingeniera].
n0(n0(ingenieras), [[f,_,p],[com,anim]]) --> [ingenieras].
n0(n0(observatorio), [[m,_,s],[com,inanim]]) --> [observatorio].
n0(n0(observatorios), [[m,_,p],[com,inanim]]) --> [observatorios].
n0(n0(telescopio), [[m,_,s],[com,inanim]]) --> [telescopio].
n0(n0(telescopios), [[m,_,p],[com,inanim]]) --> [telescopios].
n0(n0(biblioteca), [[f,_,s],[com,inanim]]) --> [biblioteca].
n0(n0(universidad), [[f,_,s],[com,inanim]]) --> [universidad].
n0(n0(laboratorio), [[m,_,s],[com,inanim]]) --> [laboratorio].

%   NOMBRES PROPIOS
n0(n0(noam), [[m,3,s],[prop,anim]]) --> [noam].
n0(n0(alan), [[m,3,s],[prop,anim]]) --> [alan].
n0(n0(irene), [[f,3,s],[prop,anim]]) --> [irene].
n0(n0(bárbara), [[f,3,s],[prop,anim]]) --> [barbara].
n0(n0(boston), [[m,3,s],[prop,inanim]]) --> [boston].
n0(n0(londres), [[m,3,s],[prop,inanim]]) --> [londres].

% determinante Features = [[caso],[rasgos phi],[rasgos semánticos],[marco de subcategorización],[wh]]
%   DETERMINANTES NULOS
d0(d0([]), [[nom,obl],[_,3,_],[prop,_],[np],[-wh]]) --> [].
d0(d0([]), [[acus],[_,3,_],[prop,inanim],[np],[-wh]]) --> [].
d0(d0(pro), [[nom],[_,_,_],[_,_],[],[-wh]]) --> [].

%   PRONOMBRES PERSONALES (PRIMERA PERSONA)
d0(d0(yo), [[nom],[_,1,s],[prop,anim],[],[-wh]]) --> [yo].
d0(d0(me), [[acus],[_,1,s],[prop,anim],[v0],[-wh]]) --> [me].			% clítico
d0(d0(mí), [[obl],[_,1,s],[prop,anim],[],[-wh]]) --> [mí].
d0(d0(nosotros), [[nom,obl],[_,1,p],[prop,anim],[],[-wh]]) --> [nosotros].
d0(d0(nosotras), [[nom,obl],[f,1,p],[prop,anim],[],[-wh]]) --> [nosotras].
d0(d0(nos), [[acus],[m,1,p],[prop,anim],[v0],[-wh]]) --> [nos].			% clítico

%   PRONOMBRES PERSONALES (SEGUNDA PERSONA)
d0(d0(tú), [[nom],[_,2,s],[prop,anim],[],[-wh]]) --> [tú].
d0(d0(te), [[acus],[_,2,s],[prop,anim],[v0],[-wh]]) --> [te].			% clítico
d0(d0(ti), [[obl],[_,2,s],[prop,anim],[],[-wh]]) --> [ti].
d0(d0(ustedes), [[nom,obl],[_,2,p],[prop,anim],[],[-wh]]) --> [ustedes].
d0(d0(los), [[acus],[m,2,p],[prop,anim],[v0],[-wh]]) --> [los].			% clítico
d0(d0(las), [[acus],[f,2,p],[prop,anim],[v0],[-wh]]) --> [las].			% clítico

%   PRONOMBRES PERSONALES (TERCERA PERSONA)
d0(d0(él), [[nom,obl],[m,3,s],[prop,anim],[],[-wh]]) --> [él].
d0(d0(ella), [[nom,obl],[f,3,s],[prop,anim],[],[-wh]]) --> [ella].
d0(d0(ellos), [[nom,obl],[m,3,p],[prop,anim],[],[-wh]]) --> [ellos].
d0(d0(ellas), [[nom,obl],[f,3,p],[prop,anim],[],[-wh]]) --> [ellas].
d0(d0(lo), [[acus],[m,3,s],[_,_],[v0],[-wh]]) --> [lo].                % cliticos, igual para comunes
d0(d0(la), [[acus],[f,3,s],[_,_],[v0],[-wh]]) --> [la].
d0(d0(los), [[acus],[m,3,p],[_,_],[v0],[-wh]]) --> [los].
d0(d0(las), [[acus],[f,3,p],[_,_],[v0],[-wh]]) --> [las].

%   (A) PERSONAL
d0(d0(a), [[acus],[_,_,_],[prop,anim],[np],[-wh]]) --> [a].
d0(d0(al), [[acus],[m,3,s],[com,anim],[np],[-wh]]) --> [al].
d0(d0(a_los), [[acus],[m,3,p],[com,anim],[np],[-wh]]) --> [a,los].
d0(d0(a_la), [[acus],[f,3,s],[com,anim],[np],[-wh]]) --> [a,la].
d0(d0(a_las), [[acus],[f,3,p],[com,anim],[np],[-wh]]) --> [a,las].

%   (A) PERSONAL + DETERMINANTES PERSONALES
d0(d0(a_el), [[acus],[m,3,s],[prop,anim],[],[-wh]]) --> [a,él].
d0(d0(a_ellos), [[acus],[m,3,p],[prop,anim],[],[-wh]]) --> [a,ellos].
d0(d0(a_ella), [[acus],[f,3,s],[prop,anim],[],[-wh]]) --> [a,ella].
d0(d0(a_ellas), [[acus],[f,3,p],[prop,anim],[],[-wh]]) --> [a,ellas].

%   DETERMINANTES DEFINIDOS
d0(d0(el), [[_],[m,3,s],[com,_],[np],[-wh]]) --> [el].
d0(d0(los), [[_],[m,3,p],[com,_],[np],[-wh]]) --> [los].
d0(d0(la), [[_],[f,3,s],[com,_],[np],[-wh]]) --> [la].
d0(d0(las), [[_],[f,3,p],[com,_],[np],[-wh]]) --> [las].

%   DETERMINANTES INDEFINIDOS
d0(d0(un), [[_],[m,3,s],[com,_],[np],[-wh]]) --> [un].
d0(d0(unos), [[_],[m,3,p],[com,_],[np],[-wh]]) --> [unos].
d0(d0(una), [[_],[f,3,s],[com,_],[np],[-wh]]) --> [una].
d0(d0(unas), [[_],[f,3,p],[com,_],[np],[-wh]]) --> [unas].

%	DETERMINANTES WH
d0(d0(quién), [[_],[_,3,s],[com,anim],[],[+wh]]) --> [quién].
d0(d0(quiénes), [[_],[_,3,p],[com,anim],[],[+wh]]) --> [quiénes].
d0(d0(h), [[_],[_,3,_],[_,_],[],[+wh]]) --> [].				% huella

% verbo Features = [[tiempo],[rasgos phi],[marco de subcategorización]]
%   VERBOS
v0(v0(vi), [[pret],[_,1,s],[dp,cp]]) --> [vi].
v0(v0(vimos), [[pret],[_,1,p],[dp,cp]]) --> [vimos].
v0(v0(viste), [[pret],[_,2,s],[dp,cp]]) --> [viste].
v0(v0(vieron), [[pret],[_,2,p],[dp,cp]]) --> [vieron].
v0(v0(vio), [[pret],[_,3,s],[dp,cp]]) --> [vio].
v0(v0(vieron), [[pret],[_,3,p],[dp,cp]]) --> [vieron].

v0(v0(saludé), [[pret],[_,1,s],[dp]]) --> [salude].
v0(v0(saludamos), [[pret],[_,1,p],[dp]]) --> [saludamos].
v0(v0(saludaste), [[pret],[_,2,s],[dp]]) --> [saludaste].
v0(v0(saludaron), [[pret],[_,2,p],[dp]]) --> [saludaron].
v0(v0(saludó), [[pret],[_,3,s],[dp]]) --> [saludo].
v0(v0(saludaron), [[pret],[_,3,p],[dp]]) --> [saludaron].

v0(v0(dije), [[pret],[_,1,s],[cp]]) --> [dije].
v0(v0(dijimos), [[pret],[_,1,p],[cp]]) --> [dijimos].
v0(v0(dijiste), [[pret],[_,2,s],[cp]]) --> [dijiste].
v0(v0(dijeron), [[pret],[_,2,p],[cp]]) --> [dijeron].
v0(v0(dijo), [[pret],[_,3,s],[cp]]) --> [dijo].
v0(v0(dijeron), [[pret],[_,3,p],[cp]]) --> [dijeron].

v0(v0(pregunté), [[pret],[_,1,s],[cp]]) --> [pregunté].
v0(v0(preguntamos), [[pret],[_,1,p],[cp]]) --> [preguntamos].
v0(v0(preguntaste), [[pret],[_,2,s],[cp]]) --> [preguntaste].
v0(v0(preguntaron), [[pret],[_,2,p],[cp]]) --> [preguntaron].
v0(v0(preguntó), [[pret],[_,3,s],[cp]]) --> [preguntó].
v0(v0(preguntaron), [[pret],[_,3,p],[cp]]) --> [preguntaron].

v0(v0(estaba), [[pret],[_,1,s],[_]]) --> [estaba].
v0(v0(estabámos), [[pret],[_,1,p],[_]]) --> [estábamos].
v0(v0(estabas), [[pret],[_,2,s],[_]]) --> [estabas].
v0(v0(estaban), [[pret],[_,2,p],[_]]) --> [estaban].
v0(v0(estaba), [[pret],[_,3,s],[_]]) --> [estaba].
v0(v0(estaban), [[pret],[_,3,p],[_]]) --> [estaban].

v0(v0(vivía), [[pret],[_,1,s],[pp]]) --> [vivía].
v0(v0(vivíamos), [[pret],[_,1,p],[pp]]) --> [vivíamos].
v0(v0(viviste), [[pret],[_,2,s],[pp]]) --> [vivías].
v0(v0(vivían), [[pret],[_,2,p],[pp]]) --> [vivían].
v0(v0(vivía), [[pret],[_,3,s],[pp]]) --> [vivía].
v0(v0(vivían), [[pret],[_,3,p],[pp]]) --> [vivían].

% adjetivo Features = [rasgos phi]
%   ADJETIVOS
adj0(adj0(famoso), [m,_,s]) --> [famoso].
adj0(adj0(famosos), [m,_,p]) --> [famosos].
adj0(adj0(famosa), [f,_,s]) --> [famosa].
adj0(adj0(famosas), [f,_,p]) --> [famosas].

adj0(adj0(borracho), [m,_,s]) --> [borracho].
adj0(adj0(borrachos), [m,_,p]) --> [borrachos].
adj0(adj0(borracha), [f,_,s]) --> [borracha].
adj0(adj0(borrachas), [f,_,p]) --> [borrachas].

adj0(adj0(simpático), [m,_,s]) --> [simpático].
adj0(adj0(simpáticos), [m,_,p]) --> [simpáticos].
adj0(adj0(simpática), [f,_,s]) --> [simpática].
adj0(adj0(simpáticas), [f,_,p]) --> [simpáticas].

% adverbio Features = [wh]
%   ADVERBIOS
adv0(adv0(ayer), [-wh]) --> [ayer].
adv0(adv0(hoy), [-wh]) --> [hoy].
adv0(adv0(cómo), [+wh]) --> [cómo].
adv0(adv0(cuándo), [+wh]) --> [cuándo].
adv0(adv0(dónde), [+wh]) --> [dónde].
%adv0(adv0(h), [+wh]) --> [].

%   INTENSIFICADORES
int0(int0(muy), []) --> [muy].

%   PREPOSICIONES
p0(p0(para), []) --> [para].
p0(p0(con), []) --> [con].
p0(p0(en), []) --> [en].
p0(p0(de), []) --> [de].
p0(p0(a), []) --> [a].

%   INFLECCION
i0(i0(fin), []) --> [].

% complementante Features = [[interrogativa], [wh]]
%   COMPLEMENTANTES
%c0(c0([]), []) --> [].
%c0(c0(que), []) --> [que].
%c0(c0(si), []) --> [si].
c0(c0([]), [[-q], [-wh]]) --> [].
c0(c0([]), [[+q], [+wh]]) --> [].
c0(c0([]), [[-q], [+wh]]) --> [].
c0(c0(que), [[-q], [-wh]]) --> [que].
c0(c0(que), [[-q], [+wh]]) --> [que].
c0(c0(si), [[-q], [-wh]]) --> [si].

%   CONECTORES
con0(con0(y), []) --> [y].
con0(con0(o), []) --> [o].
con0(con0(e), []) --> [e].
con0(con0(u), []) --> [u].


%-----------Reglas sintácticas-----------

cbar(cbar(C,IP), [T,Q,Wh]) --> c0(C,[Q,Wh]), ip(IP,[T,Wh]).
cp(cp(Cbar), [T,Q,Wh]) --> cbar(Cbar,[T,Q,Wh]), {member(-q,Q)},  {member(-wh,Wh)}.
cp(cp(Cbar), [T,Q,[+wh]]) --> cbar(Cbar,[T,Q,[+wh]]).           %clausula relativa
cp(cp(DP,Cbar), [T,[-q],[+wh]]) --> dp(DP,[_,_,[+wh]]), cbar(Cbar,[T,[-q],[+wh]]).           %pregunta
%cp(cp(AdvP,Cbar), [Features,Q,Wh]) --> advp(AdvP,[+wh]), cbar(Cbar,[Features,Q,Wh]), {member(+q,Q)}.      %pregunta
cp(cp(PP,Cbar), [T,Q,Wh]) --> pp(PP,[Wh]), cbar(Cbar,[T,Q,Wh]), {member(+q,Q)}.             %pregunta
cp(CP) --> cp(CP,_).

ibar(ibar(I,VP), [T,Phi,Wh]) --> i0(I,_), vp(VP,[T,Phi,Wh]).
ip(ip(DP,Ibar), [T,[-wh]]) --> dp(DP,[Case,Phi,[-wh]]), ibar(Ibar,[T,Phi,[-wh]]), {member(nom,Case)}.
ip(ip(DP,Ibar), [T,[+wh]]) --> dp(DP,[Case,Phi,[-wh]]), ibar(Ibar,[T,Phi,[+wh]]), {member(nom,Case)}.
ip(ip(DP,Ibar), [T,[+wh]]) --> dp(DP,[Case,Phi,[+wh]]), ibar(Ibar,[T,Phi,[-wh]]), {member(nom,Case)}.
%ip(ip(AdvP,IP), T) --> advp(AdvP,_), ip(IP,T).                  %adjuntos
%ip(ip(IP,AdvP), T) --> ip(IP,T), advp(AdvP,_).                  %adjuntos

v0(v0(D,V), [T,Phi,[]]) --> d0(D,[[acus],_,_,[v0],[-wh]]), v0(V,[T,Phi,Subcat]), {member(dp,Subcat)}. %cliticos
vbar(vbar(V), [T,Phi,[-wh]]) --> v0(V,[T,Phi,[]]).                    %intransitivos
vbar(vbar(V,CP), [T,Phi,[-wh]]) --> v0(V,[T,Phi,Subcat]), cp(CP,_), {member(cp,Subcat)}.
vbar(vbar(V,AdjP), [T,Phi,[-wh]]) --> v0(V,[T,Phi,Subcat]), adjp(AdjP,Phi), {member(adjp,Subcat)}.
vbar(vbar(V,PP), [T,Phi,[-wh]]) --> v0(V,[T,Phi,Subcat]), pp(PP,[-wh]), {member(pp,Subcat)}.
vbar(vbar(V,DP), [T,Phi,[-wh]]) --> v0(V,[T,Phi,Subcat]), dp(DP,[[acus],_,[-wh]]), {member(dp,Subcat)}.
vp(vp(Vbar), Features) --> vbar(Vbar,Features).
vp(vp(VP,PP), Features) --> vp(VP,Features), pp(PP,[-wh]).          %adjuntos
vp(vp(VP,AdvP), Features) --> vp(VP,Features), advp(AdvP,[-wh]).    %adjuntos
%vp(vp(AdvP,VP), Features) --> advp(AdvP,_), vp(VP,Features).    %adjuntos

dbar(dbar(D), [Case,Phi,Wh]) --> d0(D,[Case,Phi,[prop,anim],[],Wh]), {member(-wh,Wh)}.  %det pronominal
dbar(dbar(D), [Case,Phi,Wh]) --> d0(D,[Case,Phi,_,[],Wh]), {member(+wh,Wh)}.  %det wh
dbar(dbar(D,NP), [Case,Phi,[-wh]]) --> d0(D,[Case,Phi,Anim,[np],[-wh]]), np(NP,[Phi,Anim]).
dp(dp(Dbar), Features) --> dbar(Dbar,Features).
dp(dp(DP,ConP), [[Case],[Gen,3,p],Wh]) --> dp(DP,[Case1,[Gen,Pers1,_],Wh]), conp(ConP,[[Case2,[Gen,Pers2,_]],[dp]]), {not(Pers1 is 1;Pers2 is 1)}, {sharemember(Case,Case1,Case2)}.  %adjuntos
dp(dp(DP,ConP), [[Case],[_,1,p],Wh]) --> dp(DP,[Case1,[_,Pers1,_],Wh]), conp(ConP,[[Case2,[_,Pers2,_]],[dp]]), {Pers1 is 1;Pers2 is 1}, {sharemember(Case,Case1,Case2)}.                                 %adjuntos
dp(dp(DP,ConP), [[Case],[m,3,p],Wh]) --> dp(DP,[Case1,[Gen1,Pers1,_],Wh]), conp(ConP,[[Case2,[Gen2,Pers2,_]],[dp]]), {not(Pers1 is 1;Pers2 is 1)}, {Gen1 \== Gen2}, {sharemember(Case,Case1,Case2)}.       %adjuntos

% regla auxiliar para asignar rasgos a determinantes conectados
sharemember(Member, List1, List2) :- member(Member,List1), member(Member,List2).

nbar(nbar(N), Features) --> n0(N,Features).
nbar(nbar(N,CP), Features) --> n0(N,Features), cp(CP,[_,_,[+wh]]).
np(np(Nbar), Features) --> nbar(Nbar,Features).
np(np(NP,PP), Features) --> np(NP,Features), pp(PP,[-wh]).          %adjuntos
np(np(NP,AdjP), [Phi,Anim]) --> np(NP,[Phi,Anim]), adjp(AdjP,Phi).                              %adjuntos
np(np(AdjP,NP), [Phi,Anim]) --> adjp(AdjP,Phi), np(NP,[Phi,Anim]).                              %adjuntos
np(np(NP,ConP), [[Gen,_,p],_]) --> np(NP,[[Gen,_,Num],_]), conp(ConP,[[[Gen,_,Num],_],[np]]).   %adjuntos
np(np(NP,ConP), [[m,_,p],_]) --> np(NP,[[Gen1,_,_],_]), conp(ConP,[[[Gen2,_,_],_],[np]]), {not(Gen1==Gen2)}.   %adjuntos

advbar(advbar(Adv), Wh) --> adv0(Adv,Wh). 
advp(advp(Advbar), Wh) --> advbar(Advbar,Wh).
advp(advp(AdvP,ConP), Wh) --> advp(AdvP,Wh), conp(ConP,[_,[advp]]).                               %adjuntos

adjbar(adjbar(Adj), Phi) --> adj0(Adj,Phi).
adjp(adjp(Adjbar), Phi) --> adjbar(Adjbar,Phi).
adjp(adjp(Int,Adjbar), Phi) --> int0(Int,_), adjbar(Adjbar,Phi). %adjuntos
adjp(adjp(AdjP,ConP), Features) --> adjp(AdjP,Features), conp(ConP,[Features,[adjp]]).          %adjuntos

pbar(pbar(P,DP), Wh) --> p0(P,_), dp(DP,[Case,_,Wh]), {member(obl,Case)}.
pbar(pbar(P,AdvP), Wh) --> p0(P,_), advp(AdvP,Wh).
pp(pp(Pbar), Wh) --> pbar(Pbar,Wh).
pp(pp(PP,ConP), Wh) --> pp(PP,Wh), conp(ConP,[_,[pp]]).           %adjuntos

conbar(conbar(Con,DP), [Features,[dp]]) --> con0(Con,_), dp(DP,Features).
conbar(conbar(Con,NP), [Features,[np]]) --> con0(Con,_), np(NP,Features).
conbar(conbar(Con,AdjP), [Features,[adjp]]) --> con0(Con,_), adjp(AdjP,Features).
conbar(conbar(Con,AdvP), [Features,[advp]]) --> con0(Con,_), advp(AdvP,Features).
conbar(conbar(Con,PP), [Features,[pp]]) --> con0(Con,_), pp(PP,Features).
conp(conp(Conbar), Features) --> conbar(Conbar,Features).


