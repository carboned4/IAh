;Projecto IA 1º Parte
;Grupo:
;Artur Jose Fonseca					Nº75456 
;Andre Filipe Pardal Pires			Nº76046
;Miguel de Oliveira Melicia Martins Nº76102

;=========================== FUNCOES AUXILIARES =============================
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
			

;========================== ESTRUTURAS DE DADOS ============================

; 						TIPO RESTRICAO
(defstruct restricao (lista-var NIL) (predicado-restr NIL))

; cria-restricao(lista predicado) - 
(defun cria-restricao (lista predicado)
	(let ((restricao (make-restricao :lista-var lista :predicado-restr predicado))) 
		restricao))

; restricao-variaveis(restricao) - Returns the list of envolver variables.		
(defun restricao-variaveis (restricao)
	(restricao-lista-var restricao))

; restricao-funcao-validacao(restricao) -  Return function associate with restriction.
(defun restricao-funcao-validacao (restricao)
	(restricao-predicado-restr restricao))


	
; 						TIPO PSR

(defstruct psr (lista-var NIL) (lista-dom NIL) (lista-restr NIL) (lista-atr NIL))

;ATENCAO: lista-atr tem uma lista de pares ((NIL . "aa") ...) em que o primeiro elemento do par
;e a atribuicao e o segundo a variavel.

;###Constructor###
; cria-psr(lista lista lista) - Create PSR.
(defun cria-psr (lista-v lista-d lista-r)
	(let ((res ()))
		(dolist (el lista-v NIL)	
			(setf res (cons (cons NIL el) res)))
		(setf res (reverse res))			
		(let ((psr (make-psr :lista-var lista-v :lista-dom lista-d :lista-restr lista-r :lista-atr res))) 
			psr)))	
	
					
;###Functions###					
; psr-atributos(psr) -
(defun psr-atribuicoes (psr)
	(let ((lista-v (psr-lista-var psr)) (lista-d (psr-lista-dom psr)) (res ()))
		(loop do
		  (setf res (junta res (make-comb (first lista-v) () (first lista-d))))
		  (setf lista-v (rest lista-v))
		  (setf lista-d (rest lista-d))
		 while (not(null lista-v)))
		 res))

; psr-variaveis-todas(psr) -		 
(defun psr-variaveis-todas (psr)
	(psr-lista-var psr))
	
; psr-variaveis-nao-atribuidas(psr) - .
(defun psr-variaveis-nao-atribuidas(psr)
	
)

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!TESTS PURPOSE ONLY!!!!!!!!!!!!!!!!!!!!!!!!!!!	
(setf p (cria-psr '("aa" "ba" "fa") '((1 2) (1 3) (2 9)) '(2 3)))

;========================= Fim Estruturas de Dados ========================



