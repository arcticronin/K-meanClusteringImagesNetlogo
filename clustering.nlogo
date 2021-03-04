globals [
  dataset
  SDWC
  KK
  best ;;for genetic alg
]

breed [points point]
breed [clusters cluster]
breed [sols sol] ;;genetic
breed [possibleclusters possiblecluster];;genetic


points-own [
  pos
  cl
  name
]

clusters-own [
  pos
]

sols-own [
  bits           ;; genetic  0 not used cluster 1 used cluster
  fitness
]

possibleclusters-own [   ;; genetic
  pos
]


;; IMPORTANT set current directory in:
;;       import_data  funcion

;;        show [cl] of points with [name = "prova2.jpg"]

to-report gen
  let b map [x ->
    ([pcolor] of x ;;mean
  )] sort patches
  report (b)
end


to-report distancia [p1 p2]
  ifelse euclidean? = true[
    report sqrt sum (map [ [x y] -> (eucl x y) ^ 2 ] p1 p2)
  ][
    report sqrt sum (map [ [x y] -> (redmean x y) ^ 2 ] p1 p2)
  ]
end

to-report eucl [p1 p2]
  report sqrt sum (map [ [x y] -> (x - y) ^ 2 ] p1 p2)
end

to-report redmean [p1 p2]
  let r (first p1 + first p2) / 2
  report sqrt (
    (2 + (r / 256)) * ((first p1 - first p2)^ 2)
     + 4 * ((first bf p1 - first bf p2)^ 2)
     + (2 + (255 - r) / 256) *  ((last p1 - last p2)^ 2)
    )
end



to import_data
  clear-all
  let data []
  let currentd  "C:\\Users\\ronin\\Documents\\UNI\\Programming\\AI\\ClusteringProject\\ImageClusterization\\"
  ;;DB is the database in a folder that i can choose from Interface
  set-current-directory word currentd DB
  file-open "names.txt"
  while [not file-at-end?][
    set data file-read-line
    import-pcolors-rgb data
    create-points 1 [
      set pos gen
      set name data
      ]
  ]
  file-close
  ask patches [set pcolor black]
end

to visual-group
  let dim length sort patches
  let clusnum length sort clusters
  let pace floor ((dim - 1) / clusnum)
  let index 0
  foreach sort clusters [ x ->
    ask x [move-to (item (index + pace ) sort patches)]
    set index index + pace
  ]
   foreach sort points [ x ->
    ask x [move-to cl]
  ]
end

to print-results
  if (not empty? sort clusters)[
    clear-output
    visual-group
    set SDWC calc-SDWC
    foreach sort clusters [ x ->
      output-print x
      output-print list "standard deviation here is = " clvariance x
      ask points with [cl = x][
        output-print name
      ]
      output-print "\n"
      output-print "\n"
    ]
]
end

to-report clvariance [ x ]  ;; calculate standard deviation in cluster x
  let cld sum map [ p ->
    (distancia p ([pos] of x)) ^ 2
    ][pos] of points with [cl = x]
  report precision (sqrt (cld /(length sort points with [cl = x]))) 3
end

to-report calc-SDWC ;; calculate standard deviation in total
  let temp 0
  foreach sort clusters [ cc ->
      set temp temp + clvariance cc
    ]
  set KK length sort clusters
  let t2 (temp / length sort clusters)
  print (list "clusters = " length sort clusters "SDWC" (precision t2 3)   "Standard deviation total" (precision temp 3) )
  report t2
end

to steppingprocedure
  clear-plot
  foreach  bf(range (Clust_Number + 1)) [ c ->
    k-medias c Iter_Number
;    let temp 0
;    foreach sort clusters [ cc ->
;      set temp temp + clvariance cc
;    ]
;    set KK c
;    set SDWC temp / length sort clusters
;    print (list "clusters = " length sort clusters "SDWC" SDWC  "Standard deviation total" temp )
    set SDWC calc-SDWC
    update-plots
  ]
end

to mstep
  clear-plot
  let temp -1
  foreach  bf(range (Clust_Number + 1)) [ c ->
    foreach (range (5))[
      k-medias c Iter_Number
      ifelse (temp = -1)
      [set temp calc-SDWC]
      [set temp min list temp calc-SDWC]
    ]
  set SDWC temp
  update-plots
  ]
end


to stampa [k] ;; print cluster color characteristics
  ifelse (k < length sort clusters)[
    set k item k sort clusters
    (foreach ([pos] of k) (range length sort patches)[ [c p] ->
      ask item p sort patches [set pcolor c]
      ]
    )
  ][
    ask patches [set pcolor black]
  ]
end



to crea-clusters [K] ;; create K random clusters (from points)
  ask clusters [die]
  ask n-of K points [
    hatch 1 [
      set breed clusters
    ]
  ]
end

to k-medias [K Iter] ;; main function
  crea-clusters K
  repeat Iter [
    K-medias-step
  ]
end

to K-medias-step
  ask points [  ;; Seleccionamos para cada Point el cluster más cercano
    let p-point pos
    set cl min-one-of clusters [distancia pos p-point]
  ]
  ask clusters [   ;; Actualizamos la posición del cluster al punto medio de sus puntos
    set pos centro ([pos] of points with [cl = myself])
  ]
end

to-report centro [lista-pos] ;; return center of a 3 x n matrix
  let d length first lista-pos
  let indices (range d)
  let ret map [ x ->
    map [y ->
      mean coords y coords x lista-pos
    ] (range 3)
  ] indices
  report ret
end

; Lista de las coordenadas i-esimas de una lista de posiciones
to-report coords [i lp]
  report map [ x -> item i x ] lp
end



;;; GENETIC


to genetically-best-k
  foreach  bf(range (Clust_Number + 1)) [ c -> ;; create possible clusters
    foreach (range (5))[
      k-medias c Iter_Number
      let startingzeroes length sort possibleclusters
      foreach sort clusters [ x ->
        create-possibleclusters 1 [ set pos [pos] of x ]
      ]
      create-sols 1 [ ;; create population
        set bits n-values startingzeroes [0]
        set bits (sentence bits (n-values (length sort possibleclusters - startingzeroes) [1]))
      ]
    ]
  ]
  ask sols [ ;;fill with zeroes to have a bit for every cluster, also the ones created later
    let fillingzeroes (length sort possibleclusters - length bits)
    set bits sentence bits (n-values fillingzeroes [0])
  ]
  print "done"
end

to-report best-k
  foreach (range 100) [
    create-next-gen
    set best min-one-of sols [fitness] ;; update-best
  ]
  report 1
end

to calc-fitness ;; calculate standard deviation filtered, turtle op
  let temp 0
  let usedclusters map last filter [ s -> first s = 1 ] (map bits (sort clusters))
  foreach usedclusters [ cc ->
      set temp temp + clvariance cc
    ]
  ;;set KK length sort clusters
  let t2 (temp / length sort clusters)
  set fitness t2
  ;;print (list "clusters = " length sort clusters "SDWC" (precision t2 3)   "Standard deviation total" (precision temp 3) )
end



;;;; GENETIC FROM  LIBRARY




to create-next-gen
  ; Hacemos una copia de las tortugas que hay en este momento.
  let generacion-anterior sols with [true]

  ; Algunas de las soluciones actuales se conseguir� por medio del cruzamiento,
  ; Se divide entre 2 porque en cada paso del bucle se generan 2 nuevas soluciones.
  let numero-cruzamientos  (floor (length (sort sols) * 70 / 100 / 2))
  ;;population lenght sort sols , 70 = crossing ratio

  repeat numero-cruzamientos
  [
    ; Se ha usado una selecci�n por torneo, con un tama�o de 3 elementos.
    ; Es decir, se toman aleatoriamente 3 soluciones de la generaci�n previa
    ; y nos quedamos con el mejor de esos 3 para la reproducci�n.

    let padre1 max-one-of (n-of 3 generacion-anterior) [fitness]
    let padre2 max-one-of (n-of 3 generacion-anterior) [fitness]

    let bits-hijo cruzamiento ([bits] of padre1) ([bits] of padre2)

    ; crea 2 hijos con sus informaciones gen�ticas
    ask padre1 [ hatch 1 [ set bits item 0 bits-hijo ] ]
    ask padre2 [ hatch 1 [ set bits item 1 bits-hijo ] ]
  ]

  ; el resto de la poblaci�n se crea por clonaci�n directa
  ; de algunos miembros seleccionados de la generaci�n anterior
  repeat ((length sort sols) - numero-cruzamientos * 2)
  [
    ask max-one-of (n-of 3 generacion-anterior) [fitness]
      [ hatch 1 ]
  ]

  ; Eliminamos toda la generaci�n anterior
  ask generacion-anterior [ die ]

  ; y sobre el resto de tortugas (generaci�n reci�n generada)...
  ask sols
  [
    ; realizamos la mutaci�n (es un proceso probabil�stico)
    mutar
    ; y actualizamos su valor de fitness
    calc-fitness
  ]
end

to-report cruzamiento [bits1 bits2]
  let punto-corte 1 + random (length bits1 - 1)
  report list (sentence (sublist bits1 0 punto-corte)
                        (sublist bits2 punto-corte length bits2))
              (sentence (sublist bits2 0 punto-corte)
                        (sublist bits1 punto-corte length bits1))
end

to mutar   ;; procedimiento de tortuga
  set bits map [ b -> ifelse-value (random-float 100.0 < 0.5) [1 - b] [b] ] ;;0.5 mutuacion ratio
               bits
end


;; ===== Medidas de diversidad

;; La medida de diversidad que se proporciona es la media de las distancias de
;; Hamming entre todos los pares de soluciones.
to-report diversidad
  let distancias []
  ask turtles [
    let bits1 bits
    ask turtles with [self > myself] [
      set distancias fput (distancia-hamming bits bits1) distancias
    ]
  ]
  ; Necesitamos conocer la mayor posible distancia de Hamming entre pares de
  ; bits de este tama�o. La siguiente f�rmula se intepreta de la siguient forma:
  ; Imaginemos una poblaci�n de N tortugas, con N par, y cada tortuga tiene un
  ; �nico bit (0 o 1). La mayor diversidad se tiene si la mitad de la poblaci�n
  ; tiene 0, y la otra mitad tiene 1 (se puede calcular por c�lculo diferencial).
  ; En este caso, hay (N / 2) * (N / 2) pares de bits que difieren.
  ; Se puede probar que esta f�rmula (tomando parte entera) tambi�n vale cuando
  ; N es impar.
  let max-posibles-distancias floor (count turtles * count turtles / 4)

  ; A partir del valor anterior podemos normalizar la medida de diversidad para que
  ; tome un valor entre 0 (poblaci�n completamente homog�nea) y 1 (heterogeneidad
  ; m�xima)
  report (sum distancias) / max-posibles-distancias
end

;; La distancia de Hamming entre dos sucesiones de bits es la fracci�n de
;; posiciones en las que difieren. Se usa MAP para comparar las sucesiones,
;; posteriormente REMOVE para quitar los resultados de igualdad, y LENGTH
;; para contar los que quedan (las diferencias).
to-report distancia-hamming [bits1 bits2]
  report (length remove true (map [ [b1 b2] -> b1 = b2 ] bits1 bits2)) / world-width
end




@#$#@#$#@
GRAPHICS-WINDOW
7
165
195
354
-1
-1
20.0
1
10
1
1
1
0
0
0
1
-4
4
-4
4
0
0
1
ticks
30.0

BUTTON
7
12
130
45
Import Data
import_data
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
6
53
130
90
K-Mean Clust_N Iter_N
k-medias Clust_Number Iter_Number\nset SDWC calc-SDWC\n\n\n\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

OUTPUT
316
13
619
437
11

BUTTON
6
98
129
131
NIL
print-results\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
141
12
264
45
Iter_Number
Iter_Number
0
100
68.0
1
1
NIL
HORIZONTAL

SLIDER
138
55
264
88
Clust_Number
Clust_Number
0
20
3.0
1
1
NIL
HORIZONTAL

PLOT
619
14
885
438
plot 1
Clusters
StandardDeviationWhithinCluster
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plotxy KK SDWC"

BUTTON
685
447
820
480
NIL
mstep
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
9
379
122
412
Euclidean?
Euclidean?
0
1
-1000

MONITOR
153
379
217
424
SDWC
precision SDWC 3
17
1
11

CHOOSER
199
165
291
210
DB
DB
"mixedDS" "flowers"
0

BUTTON
197
318
305
351
Show Next Cluster
stampa clusternum\nset clusternum min list 20 (clusternum + 1)
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
200
283
306
316
clusternum
clusternum
0
20
3.0
1
1
NIL
HORIZONTAL

MONITOR
221
379
279
424
Clusters
length sort clusters
0
1
11

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
