#lang racket

(provide (all-defined-out))

;; Același arbore de TPP obținut în etapa 1 prin aplicarea
;; transformărilor T1, T2, T3 poate fi generat folosind 
;; tupluri GH (Gopal-Hemachandra).
;;
;; Pentru o pereche oarecare (g, e), secvența GH este:
;;    g, e, g + e, g + 2e, 2g + 3e, 3g + 5e ...
;; Pentru (g, e) = (1, 1) obținem șirul lui Fibonacci.
;;
;; Primele 4 numere din secvență formează cvartetul GH:
;;    (g, e, f, h) = (g, e, g + e, g + 2e)
;;
;; Pentru un asemenea cvartet (g, e, f, h), definim:
;;    a = gh,   b = 2ef,   c = e^2 + f^2
;; și putem demonstra că (a,b,c) este un triplet pitagoreic.
;;
;; (a,b,c) este chiar TPP, dacă adăugăm condițiile:
;;    g, e, f, h prime între ele
;;    g impar
;; însă nu veți avea nevoie să faceți asemenea verificări,
;; întrucât avem la dispoziție un algoritm care generează
;; exclusiv TPP.
;;
;; Acest algoritm este foarte asemănător cu cel din etapa
;; anterioară, cu următoarele diferențe:
;;  - nodurile din arbore sunt cvartete, nu triplete
;;    (din cvartet obținem un TPP conform formulelor)
;;    (ex: (1,1,2,3) => (1*3,2*1*2,1^2+2^2) = (3,4,5))
;;  - obținem următoarea generație de cvartete folosind 
;;    trei transformări Q1, Q2, Q3 pentru cvartete, în loc
;;    de T1, T2, T3 care lucrau cu triplete
;; 
;; Q1(g,e,f,h) = (h,e,h+e,h+2e)
;; Q2(g,e,f,h) = (h,f,h+f,h+2f) 
;; Q3(g,e,f,h) = (g,f,g+f,g+2f)
;;
;; Arborele rezultat arată astfel:
;;
;;                        (1,1,2,3)
;;              ______________|______________
;;             |              |              |
;;         (3,1,4,5)      (3,2,5,7)      (1,2,3,5)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;;  (5,1,6,7) .........................................

;; Definim funcțiile Q1, Q2, Q3:
(define (Q1 g e f h) (list h e (+ h e) (+ h e e)))
(define (Q2 g e f h) (list h f (+ h f) (+ h f f)))
(define (Q3 g e f h) (list g f (+ g f) (+ g f f)))

;; Vom refolosi matricile T1, T2, T3:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Reimplementați funcția care calculează produsul scalar
; a doi vectori X și Y, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
(define (dot-product X Y)
  (foldr + 0 (map * X Y)))


; TODO
; Reimplementați funcția care calculează produsul dintre
; o matrice M și un vector V, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
(define (multiply M V)
  (map (λ (X) (dot-product X V)) M))


; TODO
; Aduceți aici (nu sunt necesare modificări) implementarea
; funcției get-transformations de la etapa 1.
; Această funcție nu este re-punctată de checker, însă este
; necesară implementărilor ulterioare.
(define (get-transformations n)
  (path (edge (- (level n 1) 1) 0 1) (edge (level n 1) 0 1) n null))

(define (level n p)
  (if (<= n 0)
      0
      (+ 1 (level (- n p) (* p 3)))))

(define (edge i acc pow)
  (if (= i 0)
      acc
      (edge (- i 1) (+ acc pow) (* pow 3))))

(define (path low high n acc)
  (if (< (- high low) 3)
      acc
      (cond [(<= n (+ low (/ (- high low) 3))) (path low (+ low (/ (- high low) 3)) n (append acc (list 1)))]
            [(and (> n (+ low (/ (- high low) 3))) (<= n (+ low (* 2 (/ (- high low) 3))))) (path (+ low (/ (- high low) 3)) (+ low (* 2 (/ (- high low) 3))) n (append acc (list 2)))]
            [else (path (+ low (* 2 (/ (- high low) 3))) high n (append acc (list 3)))])))


; TODO
; În etapa anterioară ați implementat o funcție care primea
; o listă Ts de tipul celei întoarsă de get-transformations
; și un triplet de start ppt și întorcea tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Acum dorim să generalizăm acest proces, astfel încât să
; putem reutiliza funcția atât pentru transformările de tip
; T1, T2, T3, cât și pentru cele de tip Q1, Q2, Q3.
; În acest scop operăm următoarele modificări:
;  - primul parametru este o listă de funcții Fs
;    (în loc de o listă numerică Ts)
;  - al doilea parametru reprezintă un tuplu oarecare
;    (aici modificarea este doar "cu numele", fără a schimba
;    funcționalitatea, este responsabilitatea funcțiilor din
;    Fs să primească parametri de tipul lui tuple)
; Nu folosiți recursivitate explicită (ci funcționale).
(define (apply-functional-transformations Fs tuple)
  (foldl (λ (x y) (x y)) tuple Fs))


; TODO
; Tot în spiritul abstractizării, veți defini o nouă funcție
; get-nth-tuple, care calculează al n-lea tuplu din arbore. 
; Această funcție va putea fi folosită:
;  - și pentru arborele de triplete (caz în care plecăm de la
;    (3,4,5) și avansăm via T1, T2, T3)
;  - și pentru arborele de cvartete (caz în care plecăm de la
;    (1,1,2,3) și avansăm via Q1, Q2, Q3)
; Rezultă că, în afară de parametrul n, funcția va trebui să
; primească un tuplu de start și 3 funcții de transformare a
; tuplurilor.
; Definiți get-nth-tuple astfel încât să o puteți reutiliza
; cu minim de efort pentru a defini funcțiile următoare:
;    get-nth-ppt-from-matrix-transformations
;    get-nth-quadruple
; (Hint: funcții curry)
; În define-ul de mai jos nu am precizat parametrii funcției
; get-nth-tuple pentru ca voi înșivă să decideți care este
; modul optim în care funcția să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție,
; dar asistentul va observa dacă implementarea respectă cerința.
(define get-nth-tuple
  (λ (tuple F1 F2 F3) (λ (n) (apply-functional-transformations (construct-functions F1 F2 F3 (get-transformations n)) tuple))))

(define (construct-functions F1 F2 F3 Ts)
  (map (λ (x) (cond [(= x 1) F1]
                    [(= x 2) F2]
                    [else F3])) Ts))


; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil
; (hint: aplicare parțială) o funcție care calculează al n-lea
; TPP din arbore, folosind transformările pe triplete.
(define get-nth-ppt-from-matrix-transformations
  (get-nth-tuple '(3 4 5) (λ (x) (multiply T1 x)) (λ (x) (multiply T2 x)) (λ (x) (multiply T3 x))))


; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil 
; (hint: aplicare parțială) o funcție care calculează al n-lea 
; cvartet din arbore, folosind transformările pe cvartete.
(define get-nth-quadruple
  (get-nth-tuple '(1 1 2 3) (λ (x) (apply Q1 x)) (λ (x) (apply Q2 x)) (λ (x) (apply Q3 x))))


; TODO
; Folosiți rezultatul întors de get-nth-quadruple pentru a 
; obține al n-lea TPP din arbore.
(define (get-nth-ppt-from-GH-quadruples n)
  (GH-to-ppt (get-nth-quadruple n)))

(define (GH-to-ppt V)
  (list (* (fst V) (fth V)) (* 2 (snd V) (trd V)) (+ (* (snd V) (snd V)) (* (trd V) (trd V)))))

(define (fst V)
  (car V))

(define (snd V)
  (cadr V))

(define (trd V)
  (caddr V))

(define (fth V)
  (cadddr V))