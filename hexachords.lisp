(in-package :drawer)

(defun make-absolute-list (relative-list &optional (root 0))
  (loop for step in relative-list
        with o = root
        finally (return (nconc result (list o)))
        collect (shiftf o (+ o step)) into result))

(defun make-hexachord (y-origin &key (tono 9) (semitono 4) (width (* 3/5 tono)))
  (let* ((origin (pt 0 y-origin))
         (zero (pt 0 0))
         (relative-y-list (list tono tono semitono tono tono))
         (height (reduce #'+ relative-y-list))
         (absolute-y-list (make-absolute-list relative-y-list)))
    (gr (list (lns (list origin
                         (cp origin zero (pt width 0))
                         (cp origin zero (pt width height))
                         (cp origin zero (pt 0 height)))
                   :style-update '(:fill :cancel))
              (gr (mapcar (lambda (y)
                            (ln (cp origin zero (pt 0 y))
                                (cp origin zero (pt width y))
                                :style-update '(:line-thickness :thick)))
                          absolute-y-list))
              (gr (list (ln origin
                            (cp origin zero (pt 0 height))
                            :style-update '(:line-thickness :thick))
                        (ln (cp origin zero (pt width 0))
                            (cp origin zero (pt width height))
                            :style-update '(:line-thickness :thick))))
              (gr (mapcar (lambda (sillable absolute-y)
                            (make-text sillable
                                       (cp origin zero (pt (* 1/2 width)
                                                           (+ absolute-y (* 1/8 (first relative-y-list)))))))
                          (list "ut" "re" "mi" "fa" "sol" "la")
                          absolute-y-list))))))

(defun make-grid (y-list label-list width &key (line-type :dashed))
  (gr (mapcar (lambda (y label)
                (gr (list (ln (pt 0 y) (pt width y) :style-update (list :line-type line-type))
                          (make-text label (pt 0 y) :h-align :right))))
              y-list
              label-list)))


(defun get-length (ratio)
  (vicentino-tunings:ratio->length ratio :unit-interval (expt 2 1/65)))

(progn
  (let* ((btikz (make-backend-tikz :filename "hexachords-aron-std-pyth.tex"))
         (zero (pt 0 0))
         (tono (get-length 9/8))
         (limma (get-length 256/243))
         (apotome (- tono limma))
         (grid (cp (make-grid (make-absolute-list
                               (list tono tono limma tono tono limma apotome limma tono tono))
                              (list "C" "D" "E" "F" "G" "A" "B♭" "B♮" "C" "D" "E")
                              (* 3.1 tono))
                   zero
                   (pt (- (* 1/3 tono)) 0)))
         (h-grid-step (* tono 8/9))
         (hexachord (make-hexachord 0 :tono tono :semitono limma))
         (hexachords (gr (list (cp hexachord zero (pt (* h-grid-step 0) 0))
                               (cp hexachord zero (pt (* h-grid-step 1) (+ limma (* tono 3))))
                               (cp hexachord zero (pt (* h-grid-step 2) (+ limma (* tono 2))))
                               ))))
    (draw-with-multiple-backends (list btikz)
                                 (list grid hexachords))
    (compile-tikz btikz))


  (let* ((btikz (make-backend-tikz :filename "hexachords-aron-std-mt.tex"))
         (zero (pt 0 0))
         (tono (get-length (/ 9/8 (expt 81/80 1/2))))
         (limma (get-length (* 16/15 (expt 81/80 1/4))))
         (apotome (- tono limma))
         (grid (cp (make-grid (make-absolute-list
                               (list tono tono limma tono tono limma apotome limma tono tono))
                              (list "C" "D" "E" "F" "G" "A" "B♭" "B♮" "C" "D" "E")
                              (* 3.1 tono))
                   zero
                   (pt (- (* 1/3 tono)) 0)))
         (h-grid-step (* tono 8/9))
         (hexachord (make-hexachord 0 :tono tono :semitono limma))
         (hexachords (gr (list (cp hexachord zero (pt (* h-grid-step 0) 0))
                               (cp hexachord zero (pt (* h-grid-step 1) (+ limma (* tono 3))))
                               (cp hexachord zero (pt (* h-grid-step 2) (+ limma (* tono 2))))
                               ))))
    (draw-with-multiple-backends (list btikz)
                                 (list grid hexachords))
    (compile-tikz btikz))


  (let* ((btikz (make-backend-tikz :filename "hexachords-aron-ext-pyth.tex"))
         (zero (pt 0 0))
         (tono (get-length 9/8))
         (limma (get-length 256/243))
         (apotome (- tono limma))
         (quarta (+ tono tono limma))
         (quinta (+ quarta tono))
         (diapason (+ quinta quarta))
         (ditono (+ tono tono))
         (semiditono (+ tono limma))
         (grid (cp (make-grid (make-absolute-list
                               (list tono limma apotome limma tono tono limma tono tono limma apotome limma tono tono))
                              (list "G" "A" "B♭" "B♮" "C" "D" "E" "F" "G" "A" "B♭" "B♮" "C" "D" "E")
                              (* 19 tono))
                   zero
                   (pt (- (* 4/4 tono)) (- quarta))))
         (grid-♭ (cp (make-grid (make-absolute-list
                                 (list (+ tono tono limma) tono (+ tono limma) tono (+ tono tono limma) tono))
                                (list "A♭" "D♭" "E♭" "G♭" "A♭" "D♭" "E♭")
                                (* 19 tono)
                                :line-type :dotted)
                     zero
                     (pt (- (* 3/4 tono)) (+ (- quarta) limma))))
         (grid-♯ (cp (make-grid (make-absolute-list
                                 (list tono tono (+ tono limma) tono (+ tono limma) tono tono (+ tono limma) tono))
                                (list "F♯" "G♯" "A♯" "C♯" "D♯" "F♯" "G♯" "A♯" "C♯" "D♯")
                                (* 19 tono)
                                :line-type :dotted)
                     zero
                     (pt (- (* 2/4 tono)) (+ (- quinta) apotome))))
         ;; (grid-c (cp (make-grid (make-absolute-list
         ;;                         (list ) tono (+ tono limma) tono tono (+ tono limma) tono))
         ;;             ;; C’, F’, D’, G’, B♮-’
         ;;                        (list "")
         ;;                        (* 19 tono)
         ;;                        :line-type :dotted)
         ;;             zero
         ;;             (pt (- (* 2/4 tono)) (+ (- quinta) apotome))))
         (h-grid-step (* tono 8/9))
         (hexachord (make-hexachord 0 :tono tono :semitono limma))
         ;; (hexachords (gr (list (cp hexachord zero (pt (* h-grid-step 0) 0))
         ;;                       (cp hexachord zero (pt (* h-grid-step 1) (+ limma (* tono 3))))
         ;;                       (cp hexachord zero (pt (* h-grid-step 2) (+ limma (* tono 2)))))))
         (hexachord-configuration (list (cons 0 -1)                            ; c=ut, natural
                                        (cons (- tono) -1)                     ; c=re, e♭ is fa
                                        (- (* 2 tono))               ; c=mi, d♭ is fa
                                        (- quarta)                   ; c=fa, natural
                                        (cons (- quinta) 1)          ; c=sol, B♭ is fa
                                        (cons (- (+ quinta tono)) 1) ; c=la, A♭ is fa
                                        (cons (+ tono) -1)                     ; d=ut, f♯ is mi
                                        (- semiditono)               ; d=fa, c♯ is mi
                                        (cons (+ ditono) -1)                   ; e=mi, g♯ is mi
                                        (- limma)                    ; e=fa, d♯ is mi
                                        (+ limma)                    ; f=mi, g♭ is fa
                                        (- (- quarta) limma)

                                        (+ (- quarta) apotome)       ; hypothetical ut on G♯
                                        (+ (- semiditono) apotome)   ; hypothetical ut on A♯
                                        (+ apotome)                  ; hypothetical ut on C♯
                                        (+ tono apotome)             ; hypothetical ut on D♯
                                        (+ quarta limma)             ; hypothetical ut on G♭
                                        ))
         ;; (sorted-hexachords (sort hexachord-configuration #'< :key (lambda (x)
         ;;                                                             (if (consp x)
         ;;                                                                 (car x)
         ;;                                                                 x))))
         (hexachords
           (gr (loop for origin in hexachord-configuration ; sorted-hexachords
                     for cnt from 0
                     collect (if (consp origin)
                                 (gr (list
                                      (cp hexachord zero (pt (* h-grid-step cnt)
                                                             (car origin)))
                                      (cp hexachord zero (pt (* h-grid-step cnt)
                                                             (funcall (if (plusp (cdr origin))
                                                                          #'+
                                                                          #'-)
                                                                      (car origin) diapason)))))
                                 (cp hexachord zero (pt (* h-grid-step cnt) origin)))))))
    (draw-with-multiple-backends (list btikz)
                                 (list grid grid-♭ grid-♯ hexachords))
    (compile-tikz btikz))

  (let* ((btikz (make-backend-tikz :filename "hexachords-aron-ext-mt.tex"))
         (zero (pt 0 0))
         (tono (get-length (/ 9/8 (expt 81/80 1/2))))
         (limma (get-length (* 16/15 (expt 81/80 1/4))))
         (apotome (- tono limma))
         (quarta (+ tono tono limma))
         (quinta (+ quarta tono))
         (diapason (+ quinta quarta))
         (ditono (+ tono tono))
         (semiditono (+ tono limma))
         (grid (cp (make-grid (make-absolute-list
                               (list tono limma apotome limma tono tono limma tono tono limma apotome limma tono tono))
                              (list "G" "A" "B♭" "B♮" "C" "D" "E" "F" "G" "A" "B♭" "B♮" "C" "D" "E")
                              (* 19 tono))
                   zero
                   (pt (- (* 4/4 tono)) (- quarta))))
         (grid-♭ (cp (make-grid (make-absolute-list
                                 (list (+ tono tono limma) tono (+ tono limma) tono (+ tono tono limma) tono))
                                (list "A♭" "D♭" "E♭" "G♭" "A♭" "D♭" "E♭")
                                (* 19 tono)
                                :line-type :dotted)
                     zero
                     (pt (- (* 3/4 tono)) (+ (- quarta) limma))))
         (grid-♯ (cp (make-grid (make-absolute-list
                                 (list tono tono (+ tono limma) tono (+ tono limma) tono tono (+ tono limma) tono))
                                (list "F♯" "G♯" "A♯" "C♯" "D♯" "F♯" "G♯" "A♯" "C♯" "D♯")
                                (* 19 tono)
                                :line-type :dotted)
                     zero
                     (pt (- (* 2/4 tono)) (+ (- quinta) apotome))))
         (h-grid-step (* tono 8/9))
         (hexachord (make-hexachord 0 :tono tono :semitono limma))
         ;; (hexachords (gr (list (cp hexachord zero (pt (* h-grid-step 0) 0))
         ;;                       (cp hexachord zero (pt (* h-grid-step 1) (+ limma (* tono 3))))
         ;;                       (cp hexachord zero (pt (* h-grid-step 2) (+ limma (* tono 2)))))))
         (hexachord-configuration (list (cons 0 -1)                            ; c=ut, natural
                                        (cons (- tono) -1)                     ; c=re, e♭ is fa
                                        (- (* 2 tono))               ; c=mi, d♭ is fa
                                        (- quarta)                   ; c=fa, natural
                                        (cons (- quinta) 1)          ; c=sol, B♭ is fa
                                        (cons (- (+ quinta tono)) 1) ; c=la, A♭ is fa
                                        (cons (+ tono) -1)                     ; d=ut, f♯ is mi
                                        (- semiditono)               ; d=fa, c♯ is mi
                                        (cons (+ ditono) -1)                   ; e=mi, g♯ is mi
                                        (- limma)                    ; e=fa, d♯ is mi
                                        (+ limma)                    ; f=mi, g♭ is fa
                                        (- (- quarta) limma)

                                        (+ (- quarta) apotome)       ; hypothetical ut on G♯
                                        (+ (- semiditono) apotome)   ; hypothetical ut on A♯
                                        (+ apotome)                  ; hypothetical ut on C♯
                                        (+ tono apotome)             ; hypothetical ut on D♯
                                        (+ quarta limma)             ; hypothetical ut on G♭
                                        ))
         ;; (sorted-hexachords (sort hexachord-configuration #'< :key (lambda (x)
         ;;                                                             (if (consp x)
         ;;                                                                 (car x)
         ;;                                                                 x))))
         (hexachords
           (gr (loop for origin in hexachord-configuration ;sorted-hexachords
                     for cnt from 0
                     collect (if (consp origin)
                                 (gr (list
                                      (cp hexachord zero (pt (* h-grid-step cnt)
                                                             (car origin)))
                                      (cp hexachord zero (pt (* h-grid-step cnt)
                                                             (funcall (if (plusp (cdr origin))
                                                                          #'+
                                                                          #'-)
                                                                      (car origin) diapason)))))
                                 (cp hexachord zero (pt (* h-grid-step cnt) origin)))))))
    (draw-with-multiple-backends (list btikz)
                                 (list grid grid-♭ grid-♯ hexachords))
    (compile-tikz btikz))

  )
