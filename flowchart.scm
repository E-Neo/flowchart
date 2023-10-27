(import (only (scheme base) inexact define-record-type))

(define (memf f lst)
  (cond
   ((null? lst) #f)
   ((f (car lst)) lst)
   (else (memf f (cdr lst)))))

(define-syntax let-list
  (syntax-rules ()
    ((_ (() lst)
        body ...)
     (begin body ...))
    ((_ ((x y ...) lst)
        body ...)
     (let ((x (car lst)))
       (let-list ((y ...) (cdr lst))
                 body ...)))))

(define-syntax define-datatype
  (syntax-rules ()
    ((_ <type>
        (<sub-type>
         (sub-type filed ...)
         sub-type?
         (field sub-type-field)
         ...)
        ...)
     (begin
       (define-record-type <sub-type>
         (sub-type field ...)
         sub-type?
         (field sub-type-field)
         ...)
       ...
       (define <type>
         `((sub-type ,sub-type? (,sub-type-field ...))
           ...))))))

(define-syntax case-datatype
  (syntax-rules (else)
    ((_ <type> type)
     (cond
      ((memf (lambda (lst) ((cadr lst) type)) <type>)
       (error "case not covered" type))
      (else (error (string-append "not a sub-type of "
                                  (symbol->string '<type>))
                   type))))
    ((_ <type> type
        (else body1 body2 ...))
     (cond
      ((memf (lambda (lst) ((cadr lst) type)) <type>)
       (begin body1 body2 ...))
      (else (error (string-append "not a sub-type of "
                                  (symbol->string '<type>))
                   type))))
    ((_ <type> type
        ((sub-type field ...)
         body1 body2 ...)
        clause ...)
     (cond
      ((let ((lst (assv 'sub-type <type>)))
         (cond
          (lst (cond
                (((cadr lst) type) lst)
                (else #f)))
          (else (error (string-append "<"
                                      (symbol->string 'sub-type)
                                      "> is not a sub-type of "
                                      (symbol->string '<type>))
                       'sub-type))))
       =>
       (lambda (lst)
         (let ((fields (caddr lst)))
           (cond
            ((eq? (length (list 'field ...)) (length fields))
             (let-list ((field ...) (map (lambda (f) (f type)) (caddr lst)))
                       body1 body2 ...))
            (else (error "wrong number of fields" '(sub-type field ...)))))))
      (else (case-datatype <type> type clause ...))))))

(define-syntax define-parser
  (syntax-rules ()
    ((_ (parse-type x) <type>
        (<sub-type>
         sub-type sub-type? parse-sub-type?
         (field sub-type-field parse-field)
         ...)
        ...)
     (begin
       (define-datatype <type>
         (<sub-type>
          (sub-type field ...)
          sub-type?
          (field sub-type-field)
          ...)
         ...)
       (define (parse-type x)
         (cond
          ((or (and (eqv? parse-sub-type? #f) (eqv? (car x) 'sub-type))
               parse-sub-type?)
           (sub-type parse-field ...))
          ...
          (else (error (string-append (symbol->string 'parse-type)
                                      " error")
                       x))))))))

(define (foldl f init lst)
  (cond
   ((null? lst) init)
   (else (foldl f (f (car lst) init) (cdr lst)))))

(define (string-split str sep)
  (call-with-port (open-input-string str)
    (lambda (in)
      (reverse
       (let recur ((out (open-output-string))
                   (res '()))
         (let ((c (read-char in)))
           (cond
            ((eof-object? c) (let ((tail (get-output-string out)))
                               (close-port out)
                               (cond
                                ((string=? tail "") res)
                                (else (cons tail res)))))
            ((char=? c sep) (let ((part (get-output-string out)))
                              (close-port out)
                              (recur (open-output-string)
                                     (cons part res))))
            (else (write-char c out)
                  (recur out res)))))))))

(define (string-join lst sep)
  (call-with-port (open-output-string)
    (lambda (out)
      (cond
       ((pair? lst)
        (display (car lst) out)
        (for-each (lambda (str)
                    (display sep out)
                    (display str out))
                  (cdr lst))))
      (get-output-string out))))

(define (->string x)
  (cond
   ((string? x) x)
   ((symbol? x) (symbol->string x))
   ((integer? x) (number->string x))
   ((number? x) (number->string (inexact x)))
   (else (error "cannot convert to string" x))))

(define (char-width c)
  (let ((unicode (char->integer c)))
    (cond
     ((< unicode #x7f) 1/2)
     (else 1))))

(define tab-width 2)
(define tan-theta 2)
(define grid-size 32)
(define stroke-width 1/32)
(define font-size 1/2)
(define label-font-size 1/2)
(define grid-font-size 1/4)
(define arrow-head-size 1/4)
(define min-arraw-size 1)
(define (fit-width width)
  (ceiling (max width 2)))
(define (fit-height height)
  (ceiling (max height 1)))
(define min-distance 1)
(define label-distance 1/4)
(define joint-distance 1/2)

(define (text-width-height text)
  (let ((lines (string-split text #\newline)))
    (map (lambda (x)
           (* x font-size))
         `(,(apply max
                   (map (lambda (line)
                          (call-with-port (open-input-string line)
                            (lambda (in)
                              (let recur ((width 0))
                                (let ((c (read-char in)))
                                  (cond
                                   ((eof-object? c) width)
                                   (else (recur (+ (char-width c) width)))))))))
                        lines))
           ,(length lines)))))

(define-parser (sexp->tag tag) <tag>
  (<style>
   style style? #f
   (css style-css (cdr tag)))
  (<g>
   g g? #f
   (attributes g-attributes (cadr tag))
   (children g-children (cddr tag)))
  (<line>
   line line? #f
   (x1 line-x1 (cadr tag))
   (y1 line-y1 (caddr tag))
   (x2 line-x2 (cadddr tag))
   (y2 line-y2 (car (cddddr tag)))
   (attributes line-attributes (cdr (cddddr tag))))
  (<rect>
   rect rect? #f
   (x rect-x (cadr tag))
   (y rect-y (caddr tag))
   (width rect-width (cadddr tag))
   (height rect-height (car (cddddr tag)))
   (attributes rect-attributes (cdr (cddddr tag))))
  (<path>
   path path? #f
   (d path-d (cadr tag))
   (attributes path-attributes (cddr tag)))
  (<text>
   text text? #f
   (cx text-cx (cadr tag))
   (cy text-cy (caddr tag))
   (text text-text (->string (cadddr tag)))
   (attributes text-attributes (cddddr tag)))
  ;; Extended tags
  (<stadium>
   stadium stadium? #f
   (x stadium-x (cadr tag))
   (y stadium-y (caddr tag))
   (width stadium-width (cadddr tag))
   (height stadium-height (car (cddddr tag)))
   (attributes stadium-attributes (cdr (cddddr tag))))
  (<hline>
   hline hline? #f
   (x hline-x (cadr tag))
   (y hline-y (caddr tag))
   (width hline-width (cadddr tag))
   (attributes hline-attributes (cddddr tag)))
  (<vline>
   vline vline? #f
   (x vline-x (cadr tag))
   (y vline-y (caddr tag))
   (height vline-height (cadddr tag))
   (attributes vline-attributes (cddddr tag)))
  (<arrow-head>
   arrow-head arrow-head? #f
   (x arrow-head-x (cadr tag))
   (y arrow-head-y (caddr tag))
   (a arrow-head-a (cadddr tag))
   (attributes arrow-head-attributes (cddddr tag)))
  (<darrow>
   darrow darrow? #f
   (x darrow-x (cadr tag))
   (y darrow-y (caddr tag))
   (height darrow-height (cadddr tag))
   (attributes darrow-attributes (cddddr tag)))
  (<rarrow>
   rarrow rarrow? #f
   (x rarrow-x (cadr tag))
   (y rarrow-y (caddr tag))
   (width rarrow-width (cadddr tag))
   (attributes rarrow-attributes (cddddr tag)))
  (<diamond>
   diamond diamond? #f
   (x diamond-x (cadr tag))
   (y diamond-y (caddr tag))
   (width diamond-width (cadddr tag))
   (height diamond-height (car (cddddr tag)))
   (attributes diamond-attributes (cdr (cddddr tag))))
  (<parallelogram>
   parallelogram parallelogram? #f
   (x parallelogram-x (cadr tag))
   (y parallelogram-y (caddr tag))
   (width parallelogram-width (cadddr tag))
   (height parallelogram-height (car (cddddr tag)))
   (attributes parallelogram-attributes (cdr (cddddr tag))))
  ;; Debug tags
  (<point>
   point point? #f
   (cx point-cx (cadr tag))
   (cy point-cy (caddr tag))
   (attributes point-attributes (cdddr tag)))
  (<grid>
   grid grid? #f
   (width grid-width (cadr tag))
   (height grid-height (caddr tag))))

(define flowchart-css
  (let ((stroke-width (string-append (->string (* stroke-width grid-size))
                                     "px"))
        (font-size (string-append (->string (* font-size grid-size)) "px"))
        (label-font-size (string-append (->string (* label-font-size grid-size))
                                        "px")))
    `((path (fill none)
            (stroke black)
            (stroke-width ,stroke-width))
      (rect (fill none)
            (stroke black)
            (stroke-width ,stroke-width))
      (line (stroke black)
            (stroke-width ,stroke-width))
      (text (font-family monospace)
            (font-size ,font-size))
      (tspan (alignment-baseline central))
      (.grid (stroke lightgrey))
      (.arrow-head (fill black)))))

(define (display-indent indent x)
  (display (make-string (* indent tab-width) #\space))
  (display x))

(define (display-css css indent)
  (for-each
   (lambda (rule)
     (let ((selector (car rule))
           (declarations (cdr rule)))
       (display-indent indent (->string selector))
       (display " {\n")
       (for-each
        (lambda (declaration)
          (let ((property (car declaration))
                (value (cadr declaration)))
            (display-indent (+ indent 1) (->string property))
            (display ": ")
            (display (->string value))
            (display ";\n")))
        declarations)
       (display-indent indent "}\n")))
   css))

(define (display-attributes attributes)
  (for-each (lambda (attribute)
              (let ((name (car attribute))
                    (value (cadr attribute)))
                (display #\space)
                (display (->string name))
                (display "=\"")
                (display (->string value))
                (display #\")))
            attributes))

(define (display-text str)
  (string-for-each (lambda (c)
                     (display (case c
                                ((#\") "&quot;")
                                ((#\') "&apos;")
                                ((#\<) "&lt;")
                                ((#\>) "&gt;")
                                ((#\&) "&amp;")
                                (else c))))
                   str))

(define (scale-translate-string scale translate-x translate-y)
  (string-append "scale(" (->string scale)
                 ") translate(" (->string translate-x) ", "
                 (->string translate-y) ")"))

(define (display-tag tag indent)
  (case-datatype
   <tag> (sexp->tag tag)
   ((style css)
    (display-indent indent "<style>\n")
    (display-css css (+ indent 1))
    (display-indent indent "</style>\n"))
   ((g attributes children)
    (display-indent indent "<g")
    (display-attributes attributes)
    (display ">\n")
    (for-each (lambda (child)
                (display-tag child (+ indent 1)))
              children)
    (display-indent indent "</g>\n"))
   ((line x1 y1 x2 y2 attributes)
    (display-indent indent "<line")
    (display-attributes attributes)
    (display-attributes `((x1 ,(* x1 grid-size))
                          (y1 ,(* y1 grid-size))
                          (x2 ,(* x2 grid-size))
                          (y2 ,(* y2 grid-size))))
    (display " />\n"))
   ((rect x y width height attributes)
    (display-indent indent "<rect")
    (display-attributes attributes)
    (display-attributes `((x ,(* x grid-size))
                          (y ,(* y grid-size))
                          (width ,(* width grid-size))
                          (height ,(* height grid-size))))
    (display " />\n"))
   ((path d attributes)
    (display-indent indent "<path")
    (display-attributes attributes)
    (display-attributes
     `((d ,(string-join (map (lambda (data)
                               (->string
                                (cond
                                 ((number? data) (* data grid-size))
                                 (else data))))
                             d)
                        #\space))))
    (display " />\n"))
   ((text cx cy text attributes)
    (let ((width-height (text-width-height text)))
      (let ((x (- (* cx grid-size)
                  (* 1/2 grid-size (car width-height))))
            (y0 (- (* cy grid-size)
                   (* 1/2 grid-size (cadr width-height))
                   (- (* 1/2 font-size grid-size)))))
        (display-indent indent "<text")
        (display-attributes attributes)
        (display ">\n")
        (let recur ((y y0)
                    (lines (string-split text #\newline)))
          (cond
           ((pair? lines)
            (display-indent (+ indent 1) "<tspan")
            (display-attributes `((x ,x) (y ,y)))
            (display ">")
            (display-text (car lines))
            (display "</tspan>\n")
            (recur (+ y (* font-size grid-size)) (cdr lines)))))
        (display-indent indent "</text>\n"))))
   ;; Extended tags
   ((stadium x y width height attributes)
    (display-tag `(rect ,x ,y ,width ,height
                        (rx ,(* 1/2 height grid-size))
                        (ry ,(* 1/2 height grid-size))
                        . ,attributes)
                 indent))
   ((hline x y width attributes)
    (display-tag `(line ,x ,y ,(+ x width) ,y . ,attributes) indent))
   ((vline x y height attributes)
    (display-tag `(line ,x ,y ,x ,(+ y height) . ,attributes) indent))
   ((arrow-head x y a attributes)
    (display-tag
     `(path (M
             ,x ,y
             L
             ,(+ x (* 1/2 (sqrt 3) arrow-head-size))
             ,(- y (* 1/2 arrow-head-size))
             L
             ,(+ x (* 1/2 (sqrt 3) arrow-head-size))
             ,(+ y (* 1/2 arrow-head-size))
             Z)
            (class arrow-head)
            . ,(cond
                ((zero? a) attributes)
                (else
                 (cons `(transform
                         ,(string-append
                           "rotate("
                           (string-join
                            (map (lambda (num)
                                   (->string num))
                                 `(,a
                                   ,(* x grid-size)
                                   ,(* y grid-size)))
                            ", ")
                           ")"))
                       attributes))))
     indent))
   ((darrow x y height attributes)
    (display-tag `(g ,attributes
                     (vline ,x ,y ,(- height (/ arrow-head-size 2)))
                     (arrow-head ,x ,(+ y height) -90))
                 indent))
   ((rarrow x y width attributes)
    (display-tag `(g ,attributes
                     (hline ,x ,y ,(- width (/ arrow-head-size 2)))
                     (arrow-head ,(+ x width) ,y 180))
                 indent))
   ((diamond x y width height attributes)
    (display-tag
     `(path (M
             ,x ,(+ y (/ height 2))
             L
             ,(+ x (/ width 2)) ,(+ y height)
             L
             ,(+ x width) ,(+ y (/ height 2))
             L
             ,(+ x (/ width 2)) ,y
             Z)
            . ,attributes)
     indent))
   ((parallelogram x y width height attributes)
    (display-tag
     `(path (M
             ,x ,(+ y height)
             L
             ,(+ x width (- (/ height tan-theta))) ,(+ y height)
             L
             ,(+ x width) ,y
             L
             ,(+ x (/ height tan-theta)) ,y
             Z)
            . ,attributes)
     indent))
   ;; Debug tags
   ((point cx cy attributes)
    (display-indent indent "<circle")
    (display-attributes attributes)
    (display-attributes `((cx ,(* cx grid-size))
                          (cy ,(* cy grid-size))
                          (r ,(* 1/16 grid-size))))
    (display " />\n"))
   ((grid width height)
    (display-tag
     `(g ()
         .
         ,(let recur-vline ((x 0)
                            (children '()))
            (cond
             ((> x width)
              (let recur-hline ((y 0)
                                (children children))
                (cond
                 ((> y height) children)
                 ((zero? y)
                  (recur-hline (+ y 1)
                               `((hline 0 ,y ,width (class grid)) . ,children)))
                 (else
                  (recur-hline
                   (+ y 1)
                   `((hline 0 ,y ,width (class grid))
                     (text 0 ,y ,y
                           (transform
                            ,(scale-translate-string
                              (->string (/ grid-font-size font-size))
                              (->string (* grid-size font-size))
                              (->string (* (- (/ font-size grid-font-size) 1) y
                                           grid-size)))))
                     . ,children))))))
             ((zero? x)
              (recur-vline (+ x 1)
                           `((vline ,x 0 ,height (class grid)) . ,children)))
             (else
              (recur-vline
               (+ x 1)
               `((vline ,x 0 ,height (class grid))
                 (text ,x 0 ,x
                       (transform
                        ,(scale-translate-string
                          (->string (/ grid-font-size font-size))
                          (->string (* (- (/ font-size grid-font-size) 1) x
                                       grid-size))
                          (->string (* grid-size font-size)))))
                 . ,children))))))
     indent))))

(define (display-svg svg)
  (display (string-append
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
            "<svg xmlns=\"http://www.w3.org/2000/svg\" "
            "xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n"))
  (display-tag `(style . ,flowchart-css) 1)
  (for-each (lambda (tag)
              (display-tag tag 1))
            (cdr svg))
  (display "</svg>\n"))

(define-record-type <position>
  (position x y)
  position?
  (x position-x)
  (y position-y))

(define (move-position dx dy pos)
  (position (+ (position-x pos) dx)
            (+ (position-y pos) dy)))

(define-datatype <block>
  (<basic-block>
   (basic-block kind text c-pos)
   basic-block?
   (kind basic-block-kind)
   (text basic-block-text)
   (c-pos basic-block-c-pos))
  (<darrow-block>
   (darrow-block label up-layout down-layout)
   darrow-block?
   (label darrow-block-label)
   (up-layout darrow-block-up-layout)
   (down-layout darrow-block-down-layout))
  (<rarrow-block>
   (rarrow-block label left-layout right-layout)
   rarrow-block?
   (label rarrow-block-label)
   (left-layout rarrow-block-left-layout)
   (right-layout rarrow-block-right-layout)))

(define (move-block dx dy block)
  (case-datatype
   <block> block
   ((basic-block kind text c-pos)
    (basic-block kind text (move-position dx dy c-pos)))
   ((darrow-block label up-layout down-layout)
    (darrow-block label
                  (move-layout dx dy up-layout)
                  (move-layout dx dy down-layout)))
   ((rarrow-block label left-layout right-layout)
    (rarrow-block label
                  (move-layout dx dy left-layout)
                  (move-layout dx dy right-layout)))))

(define-record-type <geometry>
  (geometry pos width height l r t b)
  geometry?
  (pos geometry-pos)
  (width geometry-width)
  (height geometry-height)
  (l geometry-l)
  (r geometry-r)
  (t geometry-t)
  (b geometry-b))

(define (move-positions dx dy poses)
  (map (lambda (pos) (move-position dx dy pos)) poses))

(define (move-geometry dx dy geo)
  (geometry (move-position dx dy (geometry-pos geo))
            (geometry-width geo) (geometry-height geo)
            (move-positions dx dy (geometry-l geo))
            (move-positions dx dy (geometry-r geo))
            (move-positions dx dy (geometry-t geo))
            (move-positions dx dy (geometry-b geo))))

(define (v-align-geometry up down)
  (let* ((up-b (car (geometry-b up)))
         (down-t (car (geometry-t down)))
         (up-dx (- (position-x down-t) (position-x up-b)))
         (down-dy (max (- (+ (position-y up-b) min-distance)
                          (position-y down-t))
                       (- (+ (position-y (geometry-pos up))
                             (geometry-height up)
                             min-distance)
                          (position-y (geometry-pos down))))))
    (cond
     ((>= up-dx 0) `((,up-dx 0) (0 ,down-dy)))
     (else `((0 0) (,(- up-dx) ,down-dy))))))

(define (h-align-geometry left right)
  (let* ((left-r (car (geometry-r left)))
         (right-l (car (geometry-l right)))
         (left-dy (- (position-y right-l) (position-y left-r)))
         (right-dx (max (- (+ (position-x left-r) min-distance)
                           (position-x right-l))
                        (- (+ (position-x (geometry-pos left))
                              (geometry-width left)
                              min-distance)
                           (position-x (geometry-pos right))))))
    (cond
     ((>= left-dy 0) `((0 ,left-dy) (,right-dx 0)))
     (else `((0 0) (,right-dx ,(- left-dy)))))))

(define-record-type <layout>
  (layout block geometry)
  layout?
  (block layout-block)
  (geometry layout-geometry))

(define (move-layout dx dy l)
  (layout (move-block dx dy (layout-block l))
          (move-geometry dx dy (layout-geometry l))))

(define (sexp->layout x)
  (case (car x)
    ;; Basic block
    ((start) (let* ((txt (cadr x))
                    (t-w-h (text-width-height txt))
                    (tw (car t-w-h))
                    (th (cadr t-w-h))
                    (w (fit-width (+ tw th)))
                    (h (fit-height (+ th font-size))))
               (layout (basic-block 'stadium txt
                                    (position (/ w 2) (/ h 2)))
                       (geometry (position 0 0) w h
                                 '() `(,(position w (/ h 2)))
                                 '() `(,(position (/ w 2) h))))))
    ((end) (let* ((txt (cadr x))
                  (t-w-h (text-width-height txt))
                  (tw (car t-w-h))
                  (th (cadr t-w-h))
                  (w (fit-width (+ tw th)))
                  (h (fit-height (+ th font-size))))
             (layout (basic-block 'stadium txt
                                  (position (/ w 2) (/ h 2)))
                     (geometry (position 0 0) w h
                               `(,(position 0 (/ h 2))) '()
                               `(,(position (/ w 2) 0)) '()))))
    ((process) (let* ((txt (cadr x))
                      (t-w-h (text-width-height txt))
                      (tw (car t-w-h))
                      (th (cadr t-w-h))
                      (w (fit-width (+ tw font-size)))
                      (h (fit-height (+ th font-size))))
                 (layout (basic-block 'rect txt
                                      (position (/ w 2) (/ h 2)))
                         (geometry (position 0 0) w h
                                   `(,(position 0 (/ h 2)))
                                   `(,(position w (/ h 2)))
                                   `(,(position (/ w 2) 0))
                                   `(,(position (/ w 2) h))))))
    ((io) (let* ((txt (cadr x))
                 (t-w-h (text-width-height txt))
                 (tw (car t-w-h))
                 (th (cadr t-w-h))
                 (w (fit-width (+ tw (* 2 (/ th tan-theta)))))
                 (h (fit-height (+ th font-size))))
            (layout (basic-block 'parallelogram txt
                                 (position (/ w 2) (/ h 2)))
                    (geometry (position 0 0) w h
                              `(,(position (/ h tan-theta 2) (/ h 2)))
                              `(,(position (- w (/ h tan-theta 2)) (/ h 2)))
                              `(,(position (/ w 2) 0))
                              `(,(position (/ w 2) h))))))
    ((decision) (let* ((txt (cadr x))
                       (t-w-h (text-width-height txt))
                       (tw (car t-w-h))
                       (th (cadr t-w-h))
                       (w (fit-width (* 2 tw)))
                       (h (fit-height (* 2 th))))
                  (layout (basic-block 'diamond txt
                                       (position (/ w 2) (/ h 2)))
                          (geometry (position 0 0) w h
                                    `(,(position 0 (/ h 2)))
                                    `(,(position w (/ h 2)))
                                    `(,(position (/ w 2) 0))
                                    `(,(position (/ w 2) h))))))
    ;; Vertical alignment block
    ((d->) (let* ((label (cadr x))
                  (up (sexp->layout (caddr x)))
                  (down (sexp->layout (cadddr x)))
                  (up-down-dx-dy (v-align-geometry (layout-geometry up)
                                                   (layout-geometry down)))
                  (up-dx-dy (car up-down-dx-dy))
                  (down-dx-dy (cadr up-down-dx-dy))
                  (up-dx (car up-dx-dy))
                  (up-dy (cadr up-dx-dy))
                  (down-dx (car down-dx-dy))
                  (down-dy (cadr down-dx-dy))
                  (new-up (move-layout up-dx up-dy up))
                  (new-down (move-layout down-dx down-dy down)))
             (layout (darrow-block label new-up new-down)
                     (let* ((up-geo (layout-geometry new-up))
                            (down-geo (layout-geometry new-down))
                            (up-pos (geometry-pos up-geo))
                            (down-pos (geometry-pos down-geo))
                            (pos (position (min (position-x up-pos)
                                                (position-x down-pos))
                                           (position-y up-pos))))
                       (geometry pos
                                 (- (max (+ (position-x up-pos)
                                            (geometry-width up-geo))
                                         (+ (position-x down-pos)
                                            (geometry-width down-geo)))
                                    (position-x pos))
                                 (- (max (+ (position-y up-pos)
                                            (geometry-height up-geo))
                                         (+ (position-y down-pos)
                                            (geometry-height down-geo)))
                                    (position-y pos))
                                 (append (geometry-l up-geo)
                                         (geometry-l down-geo))
                                 (append (geometry-r up-geo)
                                         (geometry-r down-geo))
                                 (geometry-t up-geo)
                                 (geometry-b down-geo))))))
    ;; Horizontal alignment block
    ((r->) (let* ((label (cadr x))
                  (left (sexp->layout (caddr x)))
                  (right (sexp->layout (cadddr x)))
                  (left-right-dx-dy (h-align-geometry (layout-geometry left)
                                                      (layout-geometry right)))
                  (left-dx-dy (car left-right-dx-dy))
                  (right-dx-dy (cadr left-right-dx-dy))
                  (left-dx (car left-dx-dy))
                  (left-dy (cadr left-dx-dy))
                  (right-dx (car right-dx-dy))
                  (right-dy (cadr right-dx-dy))
                  (new-left (move-layout left-dx left-dy left))
                  (new-right (move-layout right-dx right-dy right)))
             (layout (rarrow-block label new-left new-right)
                     (let* ((left-geo (layout-geometry new-left))
                            (right-geo (layout-geometry new-right))
                            (left-pos (geometry-pos left-geo))
                            (right-pos (geometry-pos right-geo))
                            (pos (position (position-x left-pos)
                                           (min (position-y left-pos)
                                                (position-y right-pos)))))
                       (geometry pos
                                 (- (max (+ (position-x left-pos)
                                            (geometry-width left-geo))
                                         (+ (position-x right-pos)
                                            (geometry-width right-geo)))
                                    (position-x pos))
                                 (- (max (+ (position-y left-pos)
                                            (geometry-height left-geo))
                                         (+ (position-y right-pos)
                                            (geometry-height right-geo)))
                                    (position-y pos))
                                 (geometry-l left-geo)
                                 (geometry-r right-geo)
                                 (append (geometry-t left-geo)
                                         (geometry-t right-geo))
                                 (append (geometry-b left-geo)
                                         (geometry-b right-geo)))))))
    (else (error "sexp->block error"))))

(define (layout->tags layout)
  (let* ((block (layout-block layout))
         (geo (layout-geometry layout))
         (pos (geometry-pos geo))
         (width (geometry-width geo))
         (height (geometry-height geo)))
    (case-datatype
     <block> block
     ((basic-block kind txt c-pos)
      `((g ()
           (,kind ,(position-x pos) ,(position-y pos) ,width ,height)
           (text ,(position-x c-pos) ,(position-y c-pos) ,txt))))
     ((darrow-block label up down)
      (append (layout->tags up)
              (cond
               ((null? label) '())
               (else (let ((label (car label))
                           (up-b (car (geometry-b (layout-geometry up))))
                           (down-t (car (geometry-t (layout-geometry down)))))
                       `((darrow ,(position-x up-b) ,(position-y up-b)
                                 ,(- (position-y down-t) (position-y up-b)))
                         .
                         ,(cond
                           ((string=? label "") '())
                           (else
                            (let* ((scale (/ label-font-size font-size))
                                   (t-w-h (map (lambda (x) (* x scale))
                                               (text-width-height label))))
                              `((text 0 0 ,label
                                      (transform
                                       ,(scale-translate-string
                                         scale
                                         (* (/ 1 scale)
                                            grid-size
                                            (+ (position-x up-b)
                                               (/ label-font-size 2)
                                               (/ (car t-w-h) 2)))
                                         (* (/ 1 scale)
                                            grid-size
                                            (+ (position-y up-b)
                                               label-distance)))))))))))))
              (layout->tags down)))
     ((rarrow-block label left right)
      (append (layout->tags left)
              (cond
               ((null? label) '())
               (else
                (let ((label (car label))
                      (left-r (car (geometry-r (layout-geometry left))))
                      (right-l (car (geometry-l (layout-geometry right)))))
                  `((rarrow ,(position-x left-r) ,(position-y left-r)
                            ,(- (position-x right-l) (position-x left-r)))
                    .
                    ,(cond
                      ((string=? label "") '())
                      (else
                       (let* ((scale (/ label-font-size font-size))
                              (t-w-h (map (lambda (x) (* x scale))
                                          (text-width-height label))))
                         `((text 0 0 ,label
                                 (transform
                                  ,(scale-translate-string
                                    scale
                                    (* (/ 1 scale)
                                       grid-size
                                       (+ (position-x left-r)
                                          (/ (car t-w-h) 2)))
                                    (* (/ 1 scale)
                                       grid-size
                                       (- (position-y left-r)
                                          label-font-size)))))))))))))
              (layout->tags right))))))

(define (layout->tags-debug layout)
  (let* ((block (layout-block layout))
         (geo (layout-geometry layout))
         (pos (geometry-pos geo))
         (width (geometry-width geo))
         (height (geometry-height geo))
         (l (geometry-l geo))
         (r (geometry-r geo))
         (t (geometry-t geo))
         (b (geometry-b geo)))
    (append (layout->tags layout)
            (foldl (lambda (poses-color res)
                     (let ((poses (car poses-color))
                           (color (cadr poses-color)))
                       (foldl (lambda (pos res)
                                (cons `(point ,(position-x pos)
                                              ,(position-y pos)
                                              (fill ,color))
                                      res))
                              res
                              poses)))
                   '()
                   `(((,pos) violet)
                     ((,(position (+ (position-x pos) width)
                                  (+ (position-y pos) height)))
                      violet)
                     (,l blue) (,r yellow) (,t red) (,b green))))))

(define demo
  '(d-> ("")
        (start "start")
        (d-> ("")
             (process "i = 0\nj = 0")
             (d-> ("")
                  (r-> ("false")
                       (d-> ("true")
                            (decision "i < 0")
                            (process "handle negative i"))
                       (r-> ("false")
                            (d-> ("true")
                                 (decision "j < 0")
                                 (process "handle negative j"))
                            (process "handle positive i, j")))
                  (end "end")))))

(define demo-svg
  (let* ((layout (move-layout 1 1 (sexp->layout demo)))
         (geo (layout-geometry layout)))
    `(svg
      (grid ,(+ 2 (ceiling (geometry-width geo)))
            ,(+ 2 (ceiling (geometry-height geo))))
      .
      ,(layout->tags-debug layout))))

(with-output-to-file "test.svg" (lambda () (display-svg demo-svg)))
