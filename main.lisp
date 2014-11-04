;Projecto IA 1 Parte
;Grupo:
;Artur Jose Fonseca					N 75456 
;Andre Filipe Pardal Pires			N 76046
;Miguel de Oliveira Melicia Martins N 76102

;(load "exemplos.fas")

;=========================== FUNCOES AUXILIARES =============================
; junta(lista lista) - Function retunrs l2 append in end of l1.
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
(defconstant SEM-RESTRICOES -1)

(defstruct var (nome NIL) (valor NAO-ATRIBUIDA) (dom NIL))
(defstruct psr (lista-var NIL) (lista-restr NIL))

;###Constructor###
; cria-psr(lista lista lista) - Create PSR.	
(defun cria-psr (lista-v lista-d lista-r)
	(let ((vars NIL) (iter-var lista-v) (iter-dom lista-d) (aux NIL))
		(loop do
			(setf vars (cons (make-var :nome (first iter-var) :dom (first iter-dom)) vars))
			(setf iter-var (rest iter-var))
			(setf iter-dom (rest iter-dom))
		while(not(null iter-var)))
		(cond ((null lista-r) (setf aux SEM-RESTRICOES))
				(T (setf aux lista-r)))
		(let ((psr (make-psr :lista-var (reverse vars) :lista-restr aux)))
			psr)))		
;#################			
					
;###Functions###					
; psr-atribuicoes(psr) - Returns a list with all "PAIRS" (var . value) of PSR.
(defun psr-atribuicoes (psr)
	(let ((res NIL) (iter-var (psr-lista-var psr)))
		(loop do
			(when (not(equal (var-valor (first iter-var)) NAO-ATRIBUIDA))
				(setf res (cons(cons (var-nome (first iter-var)) (var-valor (first iter-var))) res)))
			(setf iter-var (rest iter-var))
			while(not(null iter-var)))
	(reverse res)))

; psr-variaveis-todas(psr) - Returns a list with all variables.		 
(defun psr-variaveis-todas (psr)
	(let ((res NIL) (iter-var (psr-lista-var psr)))
		(loop do
			(setf res (cons (var-nome (first iter-var)) res))
			(setf iter-var (rest iter-var))
		while(not(null iter-var)))
	(reverse res)))
		
	
; psr-variaveis-nao-atribuidas(psr) - Returns list with all variables that have
; no value.
(defun psr-variaveis-nao-atribuidas (psr)
	(let ((res NIL) (iter-var (psr-lista-var psr)))
		(loop do
			(when (equal (var-valor (first iter-var)) NAO-ATRIBUIDA)
				(setf res (cons (var-nome (first iter-var)) res)))
			(setf iter-var (rest iter-var))
		while(not(null iter-var)))
	(reverse res)))

;psr-variavel-valor(psr variavel) - Function returns value of variable or NIL if variable
; doesnt have one.
(defun psr-variavel-valor(psr var)
	(let ((iter-var (psr-lista-var psr)))
		(dolist (ele iter-var NIL)
			(cond ((and (equal var (var-nome ele)) (equal (var-valor ele) NAO-ATRIBUIDA))
				(return NIL))
					((equal var (var-nome ele)) (return (var-valor ele)))))))
				
	
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
	(cond ((equal (psr-lista-restr psr) NAO-ATRIBUIDA) (return-from psr-variavel-restricoes NIL)))
	(let ((res NIL) (i (psr-lista-restr psr)))
		(loop do
			(when (membro var (restricao-lista-var (first i)))
				(setf res (cons (first i) res)))
			(setf i (rest i))
		while(not(null i)))
	(reverse res)))

; psr-adiciona-atribuicao! (psr var valor) - Adds a value to the var.
(defun psr-adiciona-atribuicao! (psr var valor)
	(let ((iter-var (psr-lista-var psr)))
		(loop do
			(when (equal (var-nome (first iter-var)) var)
				(setf (var-valor (first iter-var)) valor)  
				(return))
			(setf iter-var (rest iter-var))
		while(not(null iter-var)))
	NIL))
		
	
;psr-remove-atribuicao!(psr var) - Makes the var without VALUE.  
(defun psr-remove-atribuicao! (psr var)
	(let ((iter-var (psr-lista-var psr)))
		(loop do
			(when (equal (var-nome (first iter-var)) var)
				(setf (var-valor (first iter-var)) NAO-ATRIBUIDA)  
				(return))
			(setf iter-var (rest iter-var))
		while(not(null iter-var)))
	NIL))

; psr-altera-dominio!(psr var dom) - Changes var Domain.	
(defun psr-altera-dominio! (psr var dom)
	(let ((var-list (psr-lista-var psr)))
		(loop do
			(when (equal var (var-nome (first var-list)))
				(setf (var-dom (first var-list)) dom))
			(setf var-list (rest var-list))
		while(not(null var-list)))))	

; psr-completo-p(psr) - Verifies if all variables have a value.
(defun psr-completo-p(psr)
	(let ((var-list (psr-lista-var psr)))
		(loop do
			(when (equal (var-valor (first var-list)) NAO-ATRIBUIDA)
				(return-from psr-completo-p NIL))
			(setf var-list (rest var-list))
		while(not(null var-list)))
	T))

; psr-consistente-p(psr) - Verifies if the CSP is consistent (VERIFIES ALL RESTRICTIONS).
(defun psr-consistente-p(psr)
	(cond ((equal (psr-lista-restr psr) SEM-RESTRICOES) (return-from psr-consistente-p (values T 0))))
	(let ((count 0) (restr (psr-lista-restr psr)))								;All restrictions.
		(dolist (restricao restr NIL)
			(when (not(funcall (restricao-funcao-validacao restricao) psr))		;Call to restriction predicate.
				(setf count (1+ count))
				(return-from psr-consistente-p (values NIL count)))
			(setf count (1+ count)))
	(values T count)))

; psr-variavel-consistente-p(psr var) - Verifies if the variable is consistent.
(defun psr-variavel-consistente-p (psr var)
	(let ((count 0) (restr (psr-variavel-restricoes psr var)))				;Restriction affects variable.
		(cond ((null restr) (return-from psr-variavel-consistente-p (values T 0))))
		(dolist (ele restr NIL)
			(when (not(funcall (restricao-funcao-validacao ele) psr))		;Call to restriction predicate.
				(setf count (1+ count))
				(return-from psr-variavel-consistente-p (values NIL count)))
			(setf count (1+ count)))
		(values T count)))

; psr-atribuicao-consistente-p(psr var value) - Verifies if.
(defun psr-atribuicao-consistente-p(psr var valor)
	(cond ((equal (psr-lista-restr psr) SEM-RESTRICOES) (return-from psr-atribuicao-consistente-p (values T 0))))
	(let ((res NIL) (aux (psr-variavel-valor psr var)))
		(cond ((equal aux NIL) (setf aux NAO-ATRIBUIDA)))
		(psr-adiciona-atribuicao! psr var valor)
		(setf res (multiple-value-list (psr-variavel-consistente-p psr var)))
		(psr-adiciona-atribuicao! psr var aux)
		(return-from psr-atribuicao-consistente-p (values (nth 0 res) (nth 1 res)))))
		
(defun teste(psr var1 var2)
	(let ((count 0) (aux ()) (restr1 (psr-variavel-restricoes psr var1)) (restr2 (psr-variavel-restricoes psr var2)))
		(dolist (ele restr1 NIL)
			(when (and (membro var1 (restricao-variaveis ele)) (membro var2 (restricao-variaveis ele)))
				(setf count (1+ count))
				(setf aux (cons ele aux))
				(when (not(funcall (restricao-funcao-validacao ele) psr)) (return-from teste (values NIL count)))))
		(dolist (ele restr2 NIL)
			(when (and (not(membro ele aux)) (membro var1 (restricao-variaveis ele)) (membro var2 (restricao-variaveis ele)))
				(setf count (1+ count))
				(when (not(funcall (restricao-funcao-validacao ele) psr)) (return-from teste (values NIL count)))))
	(values T count)))
		
; psr-atribuicoes-consistentes-arco-p(psr var1 v1 var2 v2) - Verifies if		
(defun psr-atribuicoes-consistentes-arco-p (psr var1 v1 var2 v2)
	(cond ((equal (psr-lista-restr psr) SEM-RESTRICOES) (return-from psr-atribuicoes-consistentes-arco-p (values T 0))))
	(let ((res NIL) (aux1 (psr-variavel-valor psr var1)) (aux2 (psr-variavel-valor psr var2)))
			(cond ((equal aux1 NIL) (setf aux1 NAO-ATRIBUIDA)))
			(cond ((equal aux2 NIL) (setf aux2 NAO-ATRIBUIDA)))
			(psr-adiciona-atribuicao! psr var1 v1)
			(psr-adiciona-atribuicao! psr var2 v2)
			(setf res (multiple-value-list (teste psr var1 var2)))
			(psr-adiciona-atribuicao! psr var1 aux1)
			(psr-adiciona-atribuicao! psr var2 aux2)
			(values (nth 0 res) (nth 1 res))))
				
;========================= FIM ESTRUTURAS DE DADOS ========================

;==========================================================================

;========================= FUNCOES DO TABULEIRO ===========================

; fill-a-pix->psr(array) - Transforms a Fill-a-Pix array-problem in a PSR.
(defun fill-a-pix->psr (array)
  (let*(
        (i 0)
        (nlinhas (first(array-dimensions array)))             
        (ncolunas (second(array-dimensions array)))    
        (domList (make-list (* nlinhas ncolunas) :initial-element (list 0 1)))
        (varlist (make-list (* nlinhas ncolunas) :initial-element (list -1 -1))))        
           
	  (dotimes (a nlinhas)   
      (dotimes (b ncolunas)      
      (setf (nth i varList) (format nil "~D ~D" a b))
      (setf i (+ i 1))))
	domList
   ;(print varList)
   ;(print domList)
   
   ;(cria-psr varList domList...)
    )   
)

; psr->fill-a-pix(psr int int) - Receives a solved PSR and converts to Fill-a-Pix (array).
(defun psr->fill-a-pix(psr int1 int2)
	(let ((res (make-array (list int1 int2))) (atribuicoes (psr-atribuicoes psr)))
		(dotimes (coluna int2)
			(dotimes (linha int1)
				(setf (aref res linha coluna) (cdr (first atribuicoes)))
				(setf atribuicoes (rest atribuicoes))))
		res))
				

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
;			remove {var = value} from assignment
;	return failure

(defconstant FAILURE -1)

; procura-retrocesso-simples(psr) - Receives a PSR and search for a solution.
;(defun procura-retrocesso-simples(psr)
;	psr
;)
; procura-retrocesso-simples(atribuicao psr) - Receives a PSR and search for a solution.
(defun procura-retrocesso-simples(psr)
	(cond ((psr-completo-p psr) 
		psr))
	(let ((var (first (psr-variaveis-nao-atribuidas psr))) (res NIL))
		(dolist (atr (psr-variavel-dominio psr var) NIL)
			(cond ((psr-atribuicao-consistente-p psr var atr)
				(psr-adiciona-atribuicao! psr var atr)
				(setf res (procura-retrocesso-simples psr))
				(cond ((not (equal res FAILURE)) (return-from procura-retrocesso-simples psr)))
				(psr-remove-atribuicao! psr var))))
	FAILURE))

; resolve-simples(array) - Receives a Fill-a-Pix (array) and try solve it.
(defun resolve-simples(arr)
	(let ((res (procura-retrocesso-simples (fill-a-pix->psr arr))))
		(cond ((equal res FAILURE)
			NIL)
			(T (psr->fill-a-pix arr (array-dimension arr 0) (array-dimension arr 1))))))

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  TESTS PURPOSE ONLY  !!!!!!!!!!!!!!!!!!!!!!!!!!!

;(defvar r1)
;(defvar r2)
;(defvar p1)
;(defvar l)
	
;(setf r1 (cria-restricao '("aa" "cc" "fa" "dd") #'(lambda(psr) psr T)))
;(setf r2 (cria-restricao '("aa" "cc" "ggg") #'(lambda(psr) psr NIL)))
;(setf l (list r1 r2))

;(setf p1 (cria-psr '("1 1" "2 1" "1 2" "2 2") '((1) (1) (1) (0)) l))

;(fill-a-pix->psr #2A((1 NIL 3) (4 5 6)))
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!