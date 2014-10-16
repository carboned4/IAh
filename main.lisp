;Projecto IA
;Grupo:
;Artur Jose Fonseca					Nº75456 
;Andre Filipe Pardal Pires			Nº76046
;Miguel de Oliveira Melicia Martins Nº76102

;========================== Estruturas de Dados ============================

; 						TIPO RESTRICAO
(defstruct restricao (lista-var NIL) (predicado-restr NIL))

; cria-restricao 
(defun cria-restricao (lista predicado)
	(let ((restricao (make-restricao :lista-var lista :predicado-restr predicado))) 
		restricao))

; restricao-variaveis		
(defun restricao-variaveis (restricao)
	(restricao-lista-var restricao))

; restricao-funcao-validacao 
(defun restricao-funcao-validacao (restricao)
	(restricao-predicado-restr restricao))


; 						TIPO PSR

(defstruct psr (lista-var NIL) (lista-dom NIL) (lista-restr NIL))

; cria-psr
(defun cria-psr (lista-v lista-d lista-r)
	(let ((psr (make-psr :lista-var lista-v :lista-dom lista-d :lista-restr lista-r))) 
		psr))

; junta - funcao que retorna a junção das 2 listas dadas.
(defun junta (l1 l2)
	"Junta duas listas"
	(if (null l1)
		l2
		(cons (first l1)
			(junta (rest l1) l2))))		
		
; make-comb	- auxiliary function makes all combinations between var and all 
; elements in lista-dom and put them in lista-res.	
(defun make-comb (var lista-res lista-dom)
	(cond ((null lista-dom) 
				lista-res)
		  (T 
			(make-comb var (cons (cons var (first lista-dom)) lista-res ) (rest lista-dom)))))
			
		
; psr-atributos
(defun psr-atributos (psr)
	(let ((lista-v (psr-lista-var psr)) (lista-d (psr-lista-dom psr)) (res ()))
		(loop do
		  (setf res (junta res (make-comb (first lista-v) () (first lista-d))))
		  (setf lista-v (rest lista-v))
		  (setf lista-d (rest lista-d))
		 while (not(null lista-v)))
		 res))

; psr-variaveis-todas -		 
(defun psr-variaveis-todas (psr)
	(psr-lista-var psr))
	
; psr-variaveis-nao-atribuidas - FIX - ME ainda tem uns probs.
(defun psr-variaveis-nao-atribuidas(psr)
	(let ((lista-v (psr-lista-var psr)) (res ()))
		(loop do
			(cond ((not(boundp (first lista-v)))
				(cons (first lista-v) res)))
			(setf lista-v (rest lista-v))
		 while (not(null lista-v)))
	))
;========================= Fim Estruturas de Dados ========================



