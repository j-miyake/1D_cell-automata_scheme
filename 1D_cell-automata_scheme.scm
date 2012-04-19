;; xをn回繰り返したリストを返す
(define (replicate n x)
  (if (< n 0)
    ()
    (cons x (replicate (- n 1) x))))

;; リストliの要素のうち最初のn個からなるリストを返す
(define (take n li)
  (if (>= (length li) n)
    (if (< n 1)
      ()
      (cons (car li) (take (- n 1) (cdr li))))
    '(0)))

;; セルの状態をビットに変換するための表
(define (num2bit n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    ((= n 2) 0)
    ((= n 3) 0)
    ((= n 4) 1)
    ((= n 5) 0)
    ((= n 6) 0)
    ((= n 7) 1)))

;; リストliの要素の合計を返す
(define (sum li)
  (if (null? li)
    0
    (+ (car li) (sum (cdr li)))))

;; 隣接する3つのビットのペアからそれぞれ次の状態を作って返す
;; 例えば、リスト((0 1 0) (1 0 0) (0 0 1) ...) を リスト(2 4 1 ...) にする
(define (pairs2next_bits li)
  (map num2bit (map sum li)))

;; 与えられたビット列から、隣接する3つのビットのペアのリストを作って返す
;; 例えば、リスト(0 1 0 0 1 ...) を リスト((0 1 0) (1 0 0) (0 0 1) ...) にする
(define (bits2pairs li)
  (if (null? (cdr li))
    ()
    (cons (take 3 li) (bits2pairs (cdr li)))))

;; 与えられた状態から次の状態を生成して返す
(define (next_state st)
  (pairs2next_bits (bits2pairs (cons 0 st))))

;; 初期状態を生成して返す。横幅は63文字分とする
(define (initial_state)
  (append (replicate (num_g) 0) '(1) (replicate (num_g) 0)))

;; 画面表示のためにビットを文字に変換する
(define (symbolize_bit bit)
  (cond
    ((= bit 0) " ")
    ((= bit 1) "*")))

;; 画面表示のためにビットを文字に変換する
(define (symbolize_st st)
  (map symbolize_bit st))

;; 世代数
(define (num_g) 31)

;; 状態stをn世代進める
(define (run n st)
  (if (< n 0)
    ()
    (append (cons (- (num_g) n) (symbolize_st st)) '("\n") (run (- n 1) (next_state st)))))

;; 状態initial_stateを31世代進めて、その結果を画面に表示する
(map display (run (num_g) (initial_state)))
