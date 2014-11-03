;Projecto IA 1 Parte
;Grupo:
;Artur Jose Fonseca					N 75456 
;Andre Filipe Pardal Pires			N 76046
;Miguel de Oliveira Melicia Martins N 76102

;=========================== FUNCOES AUXILIARES =============================
; junta(lista lista) - funcao que retorna a junção das 2 listas dadas.
(defun junta (l1 l2)
	"Junta duas listas"
	(if (null l1)
		l2
		(cons (first l1)
			(junta (rest l1) l2))))

; membro(elemento lista) - Verifies if the element is in the list.
(defun membro (ele lis)
	(cond ((null lis) NIL)
		((equal ele (first lis)) T)
		(T (membro ele (rest lis)))))
;========================== ESTRUTURAS DE DADOS ============================

; 						TIPO RESTRICAO
(defstruct restricao (lista-var NIL) (predicado NIL))

; cria-restricao(lista predicado) - Creates a new restriction.
(defun cria-restricao (lista predicado)
	(let ((restricao (make-restricao :lista-var lista :predicado predicado))) 
		restricao))

		
; restricao-variaveis(restricao) - Returns the list of envolved variables.		
(defun restricao-variaveis (restricao)
	(restricao-lista-var restricao))

; restricao-funcao-validacao(restricao) -  Return function associate with restriction.
(defun restricao-funcao-validacao (restricao)
	(restricao-predicado restricao))


	
; 						TIPO PSR
(defconstant NAO-ATRIBUIDA -1)

(defstruct var (nome NIL) (valor NAO-ATRIBUIDA) (dom NIL))
(defstruct psr (lista-var NIL) (lista-restr NIL))

;###Constructor###
; cria-psr(lista lista lista) - Create PSR.	
(defun cria-psr (lista-v lista-d lista-r)
	(let ((vars NIL) (iter-var lista-v) (iter-dom lista-d))
		(loop do
			(setf vars (cons (make-var :nome (first iter-var) :dom (first iter-dom)) vars))
			(setf iter-var (rest iter-var))
			(setf iter-dom (rest iter-dom))
		while(not(null iter-var)))
		(setf vars (reverse vars))
		(let ((psr (make-psr :lista-var vars :lista-restr lista-r)))
			psr)))		
;#################			
					
;###Functions###					
; psr-atribuicoes(psr) - Returns a list with all "PAIRS" (var . value) of PSR.
(defun psr-atribuicoes (psr)
	(let ((res NIL) (iter-var (psr-lista-var psr)))
		(loop do
			(setf res (cons(cons (var-nome (first iter-var)) (var-valor (first iter-var))) res))
			(setf iter-var (rest iter-var))
			while(not(null iter-var)))
	res))

; psr-variaveis-todas(psr) - Returns a list with all variables.		 
(defun psr-variaveis-todas (psr)
	(let ((res NIL) (iter-var (psr-lista-var psr)))
		(loop do
			(setf res (cons (var-nome (first iter-var)) res))
			(setf iter-var (rest iter-var))
		while(not(null iter-var)))
	res))
		
	
; psr-variaveis-nao-atribuidas(psr) - Returns list with all variables that have
; no value.
(defun psr-variaveis-nao-atribuidas (psr)
	(let ((res NIL) (iter-var (psr-lista-var psr)))
		(loop do
			(when (equal (var-valor (first iter-var)) NAO-ATRIBUIDA)
				(setf res (cons (var-nome (first iter-var)) res)))
			(setf iter-var (rest iter-var))
		while(not(null iter-var)))
	res))

; psr-variavel-dominio(psr var) - Returns var domain.
(defun psr-variavel-dominio (psr var)
	(let ((iter-var (psr-lista-var psr)))
		(loop do
			(when (equal (var-nome (first iter-var)) var)
				(return-from psr-variavel-dominio (var-dom (first iter-var))))
			(setf iter-var (rest iter-var))
		while(not(null iter-var)))
	NIL))
	
	
; psr-variavel-restricoes(psr var) - Returns all restriction applied to var in the psr.
(defun psr-variavel-restricoes(psr var)
	(let ((res NIL) (i (psr-lista-restr psr)))
		(loop do
			(when (membro var (restricao-lista-var (first i)))
				(setf res (cons (first i) res)))
			(setf i (rest i))
		while(not(null i)))
	(reverse res)))

; psr-adiciona-atribuicao(psr var valor) - Adds a value to the var.
(defun psr-adiciona-atribuicao(psr var valor)
	(let ((iter-var (psr-lista-var psr)))
		(loop do
			(when (equal (var-nome (first iter-var)) var)
				(setf (var-valor (first iter-var)) valor)  
				(return))
			(setf iter-var (rest iter-var))
		while(not(null iter-var)))
	NIL))
		
	
;psr-remove-atribuicao(psr var) - Makes the var without VALUE.  
(defun psr-remove-atribuicao(psr var)
	(let ((iter-var (psr-lista-var psr)))
		(loop do
			(when (equal (var-nome (first iter-var)) var)
				(setf (var-valor (first iter-var)) NAO-ATRIBUIDA)  
				(return))
			(setf iter-var (rest iter-var))
		while(not(null iter-var)))
	NIL))

; psr-altera-dominio(psr var dom) - Changes var Domain.	
(defun psr-altera-dominio (psr var dom)
	(let ((var-list (psr-lista-var psr)))
		(loop do
			(when (equal var (var-nome (first var-list)))
				(write 1)
				(setf (var-dom (first var-list)) dom))
			(setf var-list (rest var-list))
		while(not(null var-list)))))	

; psr-completo(psr) - Verifies if all variables have a value.
(defun psr-completo(psr)
	(let ((var-list (psr-lista-var psr)))
		(loop do
			(when (equal (var-valor (first var-list)) NAO-ATRIBUIDA)
				(return-from psr-completo NIL))
			(setf var-list (rest var-list))
		while(not(null var-list)))
	T))

; psr-consistente(psr) - 
(defun psr-consistente(psr)

)

; psr-variavel-consistente-p(psr var) - 
(defun psr-variavel-consistente-p (psr var)
	(let ((count 0) (restr (psr-variavel-restricoes psr var)))
		(dolist (ele restr NIL)
			(when (not(funcall (restricao-funcao-validacao ele) psr))		;Call to restriction predicate.
				(setf count (1+ count))
				(return-from psr-variavel-consistente-p (values NIL count)))
			(setf count (1+ count)))
		(values T count)))

; psr-atribuicao-consistente(psr var value) - 
(defun psr-atribuicao-consistente()

)
		
; psr-atribuicoes-consistentes-arco-p(psr var1 v1 var2 v2) - 		
(defun psr-atribuicoes-consistentes-arco-p ()

)
	
;========================= FIM ESTRUTURAS DE DADOS ========================

;==========================================================================

;========================= FUNCOES DO TABULEIRO ===========================

; fill-a-pix->psr(arr) - Transforms an array in a PSR.
(defun fill-a-pix->psr ()
	 
)

;========================= FIM FUNCOES DO TABULEIRO =========================

;============================================================================

;========================= FUNCOES PARA RESOLUCAO CSP =======================

;!!!!!!!!!!!!!!!!!! ALGORITMO EM PSEUDO-CODIGO !!!!!!!!!!!!!!!!!!!!!!

;function BACKTRACKING-SEARCH(csp) returns a solution, or failure
;	return RECURSIVE-BACKTRACKING({ }, csp)

;function RECURSIVE-BACKTRACKING(assignment,csp) returns a solution, or failure
;	if assignment is complete then return assignment
;	var <- SELECT-UNASSIGNED-VARIABLE(VARIABLES[csp],assignment,csp)
;	for each value in ORDER-DOMAIN-VALUES(var,assignment,csp) do
;		if value is consistent with assignment according to CONSTRAINTS[csp] then
;			add {var = value) to assignment
;			result <- RECURSIVE-BACKTRACKING(assignment,csp)
;			if result != failure then return result
;			remove {var = value) from assignment
;	return failure

; procura-retrocesso-simples(psr) - Receives a PSR and search for a solution.
(defun procura-retrocesso-simples(psr)
	(cond ((psr-competo psr)
)

;
(defun resolve-simples(arr)
	arr
)

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!TESTS PURPOSE ONLY!!!!!!!!!!!!!!!!!!!!!!!!!!!

(defvar r1)
(defvar r2)
(defvar p1)
(defvar l)
	
(setf r1 (cria-restricao '("aa" "cc" "fa" "dd") #'(lambda(psr) psr T)))
(setf r2 (cria-restricao '("aa" "cc" "ggg") #'(lambda(psr) psr NIL)))
(setf l (list r1 r2))

(setf p1 (cria-psr '("aa" "ba" "fa" "ggg") '((1 2) (1 3) (2 9) (0 1 2 3 4)) l))

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!