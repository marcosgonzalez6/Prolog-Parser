:- use_rendering(svgtree, [list(false)]).
:- use_module(library(tabling)).
:- table np/4, dp/4, vp/4, pp/4, adjp/4, advp/4, ip/4.
:- discontiguous v0/4.

%------Reglas lexicas------

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
n0(n0(bárbara), [[f,3,s],[prop,anim]]) --> [bárbara].
n0(n0(boston), [[m,3,s],[prop,inanim]]) --> [boston].
n0(n0(londres), [[m,3,s],[prop,inanim]]) --> [londres].

%   DETERMINANTES NULOS
d0(d0([]), [[nom,obl],[_,3,_],[prop,_],[np]]) --> [].
d0(d0([]), [[acus],[_,3,_],[prop,inanim],[np]]) --> [].
d0(d0(pro), [[nom],[_,_,_],[_,_],[]]) --> [].

%   PRONOMBRES PERSONALES (PRIMERA PERSONA)
d0(d0(yo), [[nom],[_,1,s],[prop,anim],[]]) --> [yo].
d0(d0(me), [[acus],[_,1,s],[prop,anim],[v0]]) --> [me].
d0(d0(mí), [[obl],[_,1,s],[prop,anim],[]]) --> [mí].
d0(d0(nosotros), [[nom,obl],[_,1,p],[prop,anim],[]]) --> [nosotros].
d0(d0(nosotras), [[nom,obl],[f,1,p],[prop,anim],[]]) --> [nosotras].
d0(d0(nos), [[acus],[m,1,p],[prop,anim],[v0]]) --> [nos].

%   PRONOMBRES PERSONALES (SEGUNDA PERSONA)
d0(d0(tú), [[nom],[_,2,s],[prop,anim],[]]) --> [tú].
d0(d0(te), [[acus],[_,2,s],[prop,anim],[v0]]) --> [te].
d0(d0(ti), [[obl],[_,2,s],[prop,anim],[]]) --> [ti].
d0(d0(ustedes), [[nom,obl],[_,2,p],[prop,anim],[]]) --> [ustedes].
d0(d0(los), [[acus],[m,2,p],[prop,anim],[v0]]) --> [los].
d0(d0(las), [[acus],[f,2,p],[prop,anim],[v0]]) --> [las].

%   PRONOMBRES PERSONALES (TERCERA PERSONA)
d0(d0(él), [[nom,obl],[m,3,s],[prop,anim],[]]) --> [él].
d0(d0(ella), [[nom,obl],[f,3,s],[prop,anim],[]]) --> [ella].
d0(d0(ellos), [[nom,obl],[m,3,p],[prop,anim],[]]) --> [ellos].
d0(d0(ellas), [[nom,obl],[f,3,p],[prop,anim],[]]) --> [ellas].
d0(d0(lo), [[acus],[m,3,s],[_,_],[v0]]) --> [lo].                % cliticos, igual para comunes
d0(d0(la), [[acus],[f,3,s],[_,_],[v0]]) --> [la].
d0(d0(los), [[acus],[m,3,p],[_,_],[v0]]) --> [los].
d0(d0(las), [[acus],[f,3,p],[_,_],[v0]]) --> [las].

%   (A) PERSONAL
d0(d0(a), [[acus],[_,_,_],[prop,anim],[np]]) --> [a].
d0(d0(al), [[acus],[m,3,s],[com,anim],[np]]) --> [al].
d0(d0(a_los), [[acus],[m,3,p],[com,anim],[np]]) --> [a,los].
d0(d0(a_la), [[acus],[f,3,s],[com,anim],[np]]) --> [a,la].
d0(d0(a_las), [[acus],[f,3,p],[com,anim],[np]]) --> [a,las].

%   (A) PERSONAL + DETERMINANTES PERSONALES
d0(d0(a_el), [[acus],[m,3,s],[prop,anim],[]]) --> [a,él].
d0(d0(a_ellos), [[acus],[m,3,p],[prop,anim],[]]) --> [a,ellos].
d0(d0(a_ella), [[acus],[f,3,s],[prop,anim],[]]) --> [a,ella].
d0(d0(a_ellas), [[acus],[f,3,p],[prop,anim],[]]) --> [a,ellas].

%   DETERMINANTES DEFINIDOS
d0(d0(el), [[_],[m,3,s],[com,_],[np]]) --> [el].
d0(d0(los), [[_],[m,3,p],[com,_],[np]]) --> [los].
d0(d0(la), [[_],[f,3,s],[com,_],[np]]) --> [la].
d0(d0(las), [[_],[f,3,p],[com,_],[np]]) --> [las].

%   DETERMINANTES INDEFINIDOS
d0(d0(un), [[_],[m,3,s],[com,_],[np]]) --> [un].
d0(d0(unos), [[_],[m,3,p],[com,_],[np]]) --> [unos].
d0(d0(una), [[_],[f,3,s],[com,_],[np]]) --> [una].
d0(d0(unas), [[_],[f,3,p],[com,_],[np]]) --> [unas].

%   VERBOS
v0(v0(vi), [[pret],[_,1,s],[dp,cp]]) --> [vi].
v0(v0(vimos), [[pret],[_,1,p],[dp,cp]]) --> [vimos].
v0(v0(viste), [[pret],[_,2,s],[dp,cp]]) --> [viste].
v0(v0(vieron), [[pret],[_,2,p],[dp,cp]]) --> [vieron].
v0(v0(vio), [[pret],[_,3,s],[dp,cp]]) --> [vio].
v0(v0(vieron), [[pret],[_,3,p],[dp,cp]]) --> [vieron].

v0(v0(saludé), [[pret],[_,1,s],[dp]]) --> [saludé].
v0(v0(saludamos), [[pret],[_,1,p],[dp]]) --> [saludamos].
v0(v0(saludaste), [[pret],[_,2,s],[dp]]) --> [saludaste].
v0(v0(saludaron), [[pret],[_,2,p],[dp]]) --> [saludaron].
v0(v0(saludó), [[pret],[_,3,s],[dp]]) --> [saludó].
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

%   ADVERBIOS
adv0(adv0(ayer), []) --> [ayer].
adv0(adv0(hoy), []) --> [hoy].

%   INTENSIFICADORES
int0(int0(muy), []) --> [muy].

%   PREPOSICIONES
p0(p0(con), []) --> [con].
p0(p0(en), []) --> [en].
p0(p0(de), []) --> [de].
p0(p0(a), []) --> [a].

%   INFLECCION
i0(i0(fin), []) --> [].

%   COMPLEMENTANTES
c0(c0([]), []) --> [].
c0(c0(que), []) --> [que].
c0(c0(si), []) --> [si].

%   CONECTORES
con0(con0(y), []) --> [y].
con0(con0(o), []) --> [o].
con0(con0(e), []) --> [e].
con0(con0(u), []) --> [u].


%------Reglas sintacticas------

cbar(cbar(C,IP), Features) --> c0(C,_), ip(IP,Features).
cp(cp(Cbar), Features) --> cbar(Cbar,Features).
cp(CP) --> cp(CP,_).

ibar(ibar(I,VP), [T,Phi]) --> i0(I,_), vp(VP,[T,Phi]).
ip(ip(DP,Ibar), T) --> dp(DP,[Case,Phi]), ibar(Ibar,[T,Phi]), {member(nom,Case)}.
ip(ip(AdvP,IP), T) --> advp(AdvP,_), ip(IP,T).                  %adjuntos
ip(ip(IP,AdvP), T) --> ip(IP,T), advp(AdvP,_).                  %adjuntos
ip(IP) --> ip(IP,_).

v0(v0(D,V), [T,Phi,[]]) --> d0(D,[[acus],_,_,[v0]]), v0(V,[T,Phi,Subcat]), {member(dp,Subcat)}. %cliticos
vbar(vbar(V), [T,Phi]) --> v0(V,[T,Phi,[]]).                    %intransitivos
vbar(vbar(V,CP), [T,Phi]) --> v0(V,[T,Phi,Subcat]), cp(CP,_), {member(cp,Subcat)}.
vbar(vbar(V,AdjP), [T,Phi]) --> v0(V,[T,Phi,Subcat]), adjp(AdjP,Phi), {member(adjp,Subcat)}.
vbar(vbar(V,PP), [T,Phi]) --> v0(V,[T,Phi,Subcat]), pp(PP,_), {member(pp,Subcat)}.
vbar(vbar(V,DP), [T,Phi]) --> v0(V,[T,Phi,Subcat]), dp(DP,[[acus],_]), {member(dp,Subcat)}.
vp(vp(Vbar), Features) --> vbar(Vbar,Features).
vp(vp(VP,PP), Features) --> vp(VP,Features), pp(PP,_).          %adjuntos
vp(vp(VP,AdvP), Features) --> vp(VP,Features), advp(AdvP,_).    %adjuntos
vp(vp(AdvP,VP), Features) --> advp(AdvP,_), vp(VP,Features).    %adjuntos

dbar(dbar(D), [Case,Phi]) --> d0(D,[Case,Phi,[prop,anim],[]]).  %det pronominal
dbar(dbar(D,NP), [Case,Phi]) --> d0(D,[Case,Phi,Anim,[np]]), np(NP,[Phi,Anim]).
dp(dp(Dbar), Features) --> dbar(Dbar,Features).
dp(dp(DP,ConP), [[Case],[Gen,3,p]]) --> dp(DP,[Case1,[Gen,Pers1,_]]), conp(ConP,[[Case2,[Gen,Pers2,_]],[dp]]), {not(Pers1 is 1;Pers2 is 1)}, {sharemember(Case,Case1,Case2)}.  %adjuntos
dp(dp(DP,ConP), [[Case],[_,1,p]]) --> dp(DP,[Case1,[_,Pers1,_]]), conp(ConP,[[Case2,[_,Pers2,_]],[dp]]), {Pers1 is 1;Pers2 is 1}, {sharemember(Case,Case1,Case2)}.                                 %adjuntos
dp(dp(DP,ConP), [[Case],[m,3,p]]) --> dp(DP,[Case1,[Gen1,Pers1,_]]), conp(ConP,[[Case2,[Gen2,Pers2,_]],[dp]]), {not(Pers1 is 1;Pers2 is 1)}, {Gen1 \== Gen2}, {sharemember(Case,Case1,Case2)}.       %adjuntos

% regla auxiliar
sharemember(Member, List1, List2) :- member(Member,List1), member(Member,List2).

nbar(nbar(N), Features) --> n0(N,Features).
np(np(Nbar), Features) --> nbar(Nbar,Features).
np(np(NP,PP), Features) --> np(NP,Features), pp(PP,_).          %adjuntos
np(np(NP,AdjP), [Phi,Anim]) --> np(NP,[Phi,Anim]), adjp(AdjP,Phi).                              %adjuntos
np(np(AdjP,NP), [Phi,Anim]) --> adjp(AdjP,Phi), np(NP,[Phi,Anim]).                              %adjuntos             %adjuntos
np(np(NP,ConP), [[Gen,_,p],_]) --> np(NP,[[Gen,_,Num],_]), conp(ConP,[[[Gen,_,Num],_],[np]]).   %adjuntos
np(np(NP,ConP), [[m,_,p],_]) --> np(NP,[[Gen1,_,_],_]), conp(ConP,[[[Gen2,_,_],_],[np]]), {not(Gen1==Gen2)}.   %adjuntos

advbar(advbar(Adv), _) --> adv0(Adv,_).
advp(advp(Advbar), _) --> advbar(Advbar,_).
advp(advp(AdvP,ConP), _) --> advp(AdvP,_), conp(ConP,[_,[advp]]).                               %adjuntos

adjbar(adjbar(Adj), Phi) --> adj0(Adj,Phi).
adjp(adjp(Adjbar), Phi) --> adjbar(Adjbar,Phi).
adjp(adjp(Int,Adjbar), Phi) --> int0(Int,_), adjbar(Adjbar,Phi). %adjuntos
adjp(adjp(AdjP,ConP), Features) --> adjp(AdjP,Features), conp(ConP,[Features,[adjp]]).          %adjuntos

pbar(pbar(P,DP), _) --> p0(P,_), dp(DP,[Case,_]), {member(obl,Case)}.
pp(pp(Pbar), _) --> pbar(Pbar,_).
pp(pp(PP,ConP), _) --> pp(PP,_), conp(ConP,[_,[pp]]).           %adjuntos

conbar(conbar(Con,DP), [Features,[dp]]) --> con0(Con,_), dp(DP,Features).
conbar(conbar(Con,NP), [Features,[np]]) --> con0(Con,_), np(NP,Features).
conbar(conbar(Con,AdjP), [Features,[adjp]]) --> con0(Con,_), adjp(AdjP,Features).
conbar(conbar(Con,AdvP), [Features,[advp]]) --> con0(Con,_), advp(AdvP,Features).
conbar(conbar(Con,PP), [Features,[pp]]) --> con0(Con,_), pp(PP,Features).
conp(conp(Conbar), Features) --> conbar(Conbar,Features).



