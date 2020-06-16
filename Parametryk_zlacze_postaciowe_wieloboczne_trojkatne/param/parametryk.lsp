;;;;;;;; ZLACZE WIELOBOCZNE TRÓJK?TKATNE ;;;;;;;
;Kamil Martenczuk Autodesk Authorized Developer;
;;;;;;;;;;;;;; ADN ID DEPL2710 ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; 05/2020 ;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Przeliczenia katów
(defun kat (w) (* pi (/ w 180.0)))
(defun katWew (w) (* pi (/ (- 180.0 w) 180.0))) 
;Deklaracja funkcji
(defun trojk ()
  (setvar 'osmode 0)
;Menu wybór srednicy zlacza
  (initget 1 "30 35 40 45 50 55 60 65 70 75")
  (setq d (getreal "\nPodaj srednic? z??cza d (mm)[30/35/40/45/50/55/60/65/70/75]: "))
;Wgranie tablicy
  (setq zlacza (open (strcat kxx "tablice/parametryk_zlacza_dane.txt") "r"))
  (setq linia (read-line zlacza))
  (setq li (atof (substr linia 1 4)))
  (setq wyzn (atof (substr linia 4 4)))
;Przeszukanie tablicy
  (setq wi 1)
  (while (> d li)
    (setq linia (read-line zlacza))
    (setq li (atof (substr linia 1 4)))
    (setq wi (+ wi 1))
    )
  (close zlacza)
;Przypisywanie wymiarow posrednich
  (setq d2 (atof (substr linia 9 4)))
  (setq d3 (atof (substr linia 4 4)))
  (setq e1 (atof (substr linia 14 4)))
  (setq wsk (atof (substr linia 19 3)))
  (setq Pp0 (atof (substr linia 23 3)))
  (setq skala (/ d3 wyzn))
  (setq r1 (/ d2 2))
  (setq r2 (/ d3 2))
  ;Deklaracja P0
  (setq p0 (getpoint "\nWska? punkt wstawienia widoku: "))
  (command "_.zoom" "_w" (polar p0 (kat 225) 100) (polar p0 (kat 45) 100))
  ;Wyznaczanie punktów
  (command "_.layer" "_s" "0" "_lw" "0.3" "0" "")
  (setq p1 (polar p0 (kat 90) r2))
  (setq p2 (polar p0 (kat -90) r2))
  (setq p3 (polar p0 (kat 90) r1))
  (setq p4 (polar p0 (kat -90) r1))
  (setq pW1 (polar p0 (kat 180) (* d3 0.8)))
  (setq pW2 (polar p0 (kat 180) d3))
  (setq pW3 (polar p0 (kat 180) (* d3 1.2)))
  (setq pW4 (polar p2 (kat -45) (* 1.2 d3)))
  ;Wymiarowanie
  (command "_.-layer" "_n" "wymiary" "_s" "wymiary" "_color" "_T" "0,255,0" "wymiary" "")
  (command "_.dim" "_ver" p1 p2 pW3 (strcat "%%c " (rtos d3 2 1 ) "mm")
	   "_ver" p2 p3 pW2 (strcat "%%c " (rtos d 2 1 ) "mm")
	   "_ver" p3 p4 pW1 (strcat "%%c " (rtos d2 2 1 ) "mm")
	   "_ver" p1 p3 pW4 (strcat "2e1 " (rtos (* 2 e1) 2 1 ) "mm") "_exit")
  ;Wgranie bloku zlacza
  (command "_.-layer" "_n" "P3G" "_s" "P3G" "_color" "_T" "255,0,0" "P3G" "_lw" "0.3" "P3G" "")
  (command "_.insert" (strcat kxx "bloki/parametryk_blok") p0 skala skala "")
  ;Zapis Piasty i Zlacza
  (command "_.-layer" "_n" "other" "_s" "other" "_color" "_T" "100,100,100" "other" "_lw" "0.3" "other" "")
  (command "_.circle" p0 "_d" d2)
  (command "_.circle" p0 "_d" d3)
  (command "_.circle" p0 "_d" (* d3 1.3))
  (command "_.-layer" "_n" "Centers" "_s" "Centers" "_color" "_T" "100,100,100" "Centers" "_lw" "0.15" "Centers" "")
  (command "_.centermark" p1 "")
  (command "_.layer" "_s" "0" "")
  ;Wstawianie widoku 2
  (initget 1 "Tak Nie")
  (setq wid2 (getkword "\nWstawi? kolejny widok? [Tak/Nie]: "))
  (cond ((= wid2 "Tak")
	 ;Deklaracja dlugosci
	 (setq l (getreal "\nPodaj d?ugo?? (mm): "))
	 ;Rekonfiguracja punktów pierwotnych
	 (setq p5 (polar p0 (kat 90) (* 1.5 r1)))
	 (setq p6 (polar p0 (kat -90) (* 1.5 r1)))
	 (setq p7 (polar p0 (kat 90) (* skala 7.93)))
	 (setq p8 (polar p0 (kat -90) r2))
	 (setq p9 (polar p0 (kat 90) r1))
	 ;Wyznaczenie punktów
	 (setq p10 (polar p0 (kat -90) r1))
	 (setq p11 (polar p5 (kat 0) (* 1.4 d3)))
	 (setq p12 (polar p6 (kat 0) (* 1.4 d3)))
	 (setq p13 (polar p7 (kat 0) (* 1.4 d3)))
	 (setq p14 (polar p8 (kat 0) (- (* 1.4 d3) 10)))
	 (setq p15 (polar p9 (kat 0) (- (* 1.4 d3) 10)))
	 (setq p16 (polar p10 (kat 0) (* 1.4 d3)))
	 (setq p17 (polar p16 (kat 0) 30))
	 (command "_.zoom" "_w" (polar p0 (kat 225) 100) (polar p12 (kat 45) (+ 50 l)))
	 ;Zapis widoku 2
	 (command "_.layer" "_s" "other" "")
	 (command "_pline" (polar p12 (kat 0) l) "_w" 0.3 0.3 p12 p11 (polar p11 (kat 0) l) "_cl")
	 (command "_.layer" "_n" "zig" "_s" "zig" "_color" "_T" "255,0,0" "zig" "_ltype" "Amzigzag" "zig" "_lw" "0.15" "zig" "")
	 (command "_line" p15 p14 "")
	 (command "_line" (polar p14 0 (+ 20 l)) (polar p15 0 (+ 20 l)) "")
	 (command "_.layer" "_s" "P3G" "")
	 (command "_line" p15 "@10<0" "")
	 (command "_line" p14 "@10<0" "")
	 (command "_line" (polar p7 0 (- (* 1.4 d3) 10)) "@10<0" "")
	 (command "_line" (polar p7 0 (+ (* 1.4 d3) l)) "@10<0" "")
	 (command "_line" (polar p14 0 (+ 10 l)) "@10<0" "")
	 (command "_line" (polar p15 0 (+ 10 l)) "@10<0" "")
	 (command "_.layer" "_n" "sym" "_s" "sym" "_lw" "0.15" "sym" "_color" "_T" "100,100,100" "sym" "_ltype" "Center" "sym" "")
	 (command "_line" (polar p0 0 (- (* 1.3 d3) 15)) (polar p0 0 (+ (* 1.3 d3) (+ 15 l))) "")
	 ;Wymiarowanie
	 (command "_.-layer" "_s" "wymiary" "")
	 (command "_.dim" "_hor" p11 (polar p11 0 l) (polar p11 (kat 90) (/ r1 2)) (strcat "L=" (rtos l 2 1 ) "mm") "_exit")
	 (command "_.layer" "_s" "0" "")
	 )
	)
  (setvar 'osmode 1)
  )
(trojk)
(setq trojk nil)