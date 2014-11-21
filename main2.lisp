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

;###Constructor###
; cria-restricao(lista predicado) - Creates a new restriction.
(defun cria-restricao (lista predicado)
	(let ((restricao (make-restricao :lista-var lista :predicado predicado))) 
		restricao))
;#################	
		
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

; psr-variavel-valor(psr variavel) - Function returns value of variable or NIL if variable
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
	(cond ((equal (psr-lista-restr psr) SEM-RESTRICOES) (return-from psr-variavel-restricoes NIL)))
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
	(cond ((null (psr-variaveis-nao-atribuidas psr)) 
			T)
		(T 
			NIL)))

; psr-consistente-p(psr) - Verifies if the CSP is consistent (VERIFIES ALL CSP RESTRICTIONS).
(defun psr-consistente-p(psr)
	(cond ((equal (psr-lista-restr psr) SEM-RESTRICOES) (return-from psr-consistente-p (values T 0))))
	(let ((count 0) (restr (psr-lista-restr psr)))								;All restrictions.
		(dolist (restricao restr NIL)
			(when (not(funcall (restricao-funcao-validacao restricao) psr))		;Call to restriction predicate.
				(setf count (1+ count))
				(return-from psr-consistente-p (values NIL count)))
			(setf count (1+ count)))
	(values T count)))

; psr-variavel-consistente-p(psr var) - Verifies all variable restrictions.
(defun psr-variavel-consistente-p (psr var)
	(let ((count 0) (restr (psr-variavel-restricoes psr var)))				;Restriction affects variable.
		(cond ((null restr) (return-from psr-variavel-consistente-p (values T 0))))
		(dolist (ele restr NIL)
			(when (not(funcall (restricao-funcao-validacao ele) psr))		;Call to restriction predicate.
				(setf count (1+ count))
				(return-from psr-variavel-consistente-p (values NIL count)))
			(setf count (1+ count)))
		(values T count)))

; psr-atribuicao-consistente-p(psr var value) - Verifies if { var = value } maintain var restricitons consistent.
(defun psr-atribuicao-consistente-p(psr var valor)
	(cond ((equal (psr-lista-restr psr) SEM-RESTRICOES) (return-from psr-atribuicao-consistente-p (values T 0))))
	(let ((res NIL) (aux (psr-variavel-valor psr var)))
		(cond ((equal aux NIL) (setf aux NAO-ATRIBUIDA)))
		(psr-adiciona-atribuicao! psr var valor)
		(setf res (multiple-value-list (psr-variavel-consistente-p psr var)))
		(psr-adiciona-atribuicao! psr var aux)
		(return-from psr-atribuicao-consistente-p (values (nth 0 res) (nth 1 res)))))

; (Funcao Auxiliar) restricoes-conjuntas-consistentes(psr var1 var2) - Verifies all restrictions
; that have both var1 and var2.
(defun restricoes-conjuntas-consistentes(psr var1 var2)
	(let ((count 0) (aux ()) (restr1 (psr-variavel-restricoes psr var1)) (restr2 (psr-variavel-restricoes psr var2)))
		(dolist (ele restr1 NIL)
			(when (and (membro var1 (restricao-variaveis ele)) (membro var2 (restricao-variaveis ele)))
				(setf count (1+ count))
				(setf aux (cons ele aux))
				(when (not(funcall (restricao-funcao-validacao ele) psr)) (return-from restricoes-conjuntas-consistentes (values NIL count)))))
		(dolist (ele restr2 NIL)
			(when (and (not(membro ele aux)) (membro var1 (restricao-variaveis ele)) (membro var2 (restricao-variaveis ele)))
				(setf count (1+ count))
				(when (not(funcall (restricao-funcao-validacao ele) psr)) (return-from restricoes-conjuntas-consistentes (values NIL count)))))
	(values T count)))
		
; psr-atribuicoes-consistentes-arco-p(psr var1 v1 var2 v2) - Verifies if.
(defun psr-atribuicoes-consistentes-arco-p (psr var1 v1 var2 v2)
	(cond ((equal (psr-lista-restr psr) SEM-RESTRICOES) (return-from psr-atribuicoes-consistentes-arco-p (values T 0))))
	(let ((res NIL) (aux1 (psr-variavel-valor psr var1)) (aux2 (psr-variavel-valor psr var2)))
			(cond ((equal aux1 NIL) (setf aux1 NAO-ATRIBUIDA)))
			(cond ((equal aux2 NIL) (setf aux2 NAO-ATRIBUIDA)))
			(psr-adiciona-atribuicao! psr var1 v1)
			(psr-adiciona-atribuicao! psr var2 v2)
			(setf res (multiple-value-list (restricoes-conjuntas-consistentes psr var1 var2)))
			(psr-adiciona-atribuicao! psr var1 aux1)
			(psr-adiciona-atribuicao! psr var2 aux2)
			(values (nth 0 res) (nth 1 res))))
				
;========================= FIM ESTRUTURAS DE DADOS ===============================

;=================================================================================

;========================= FUNCOES DO TABULEIRO ==================================

; FUNCOES AUXILIARES

; boarders(x y nlinhas ncolunas - Returns all positions around one given (x y).
(defun boarders (x y nlinhas ncolunas)
	(let* (
	(i 0)       
	(xmin (max (- x 1) 0))
	(xmax (min (+ x 1) (- nlinhas 1)))
	(ymin (max (- y 1) 0))
	(ymax (min (+ y 1) (- ncolunas 1)))
	(boarderList (make-list (* (- (+ xmax 1) (+ xmin 0)) (- (+ ymax 1) (+ ymin 0))))))

	(loop for a from ymin to ymax
		  do(
		  loop for b from xmin to xmax      
		  do(
			 setf (nth i boarderList) (format nil "~D ~D" b a))
		  do(setf i (+ i 1))    
		  ))
	boarderList))

; cria-pred-geral(valor lista) - Create Closure used in positions that have a restriction
; between 1 and 8.
(defun cria-pred-geral (valor lista)
	(let ((aux 0) (aux2 -1) (cmp valor) (lis lista))
		#'(lambda (psr)
			(dolist (ele lis NIL)
					(when (null (psr-variavel-valor psr ele)) (setf aux2 T) (return))
					(setf aux (+ (psr-variavel-valor psr ele) aux)))
				(cond ((not(equal aux2 -1)) (setf aux2 -1) (setf aux 0) T)
					((equal aux cmp) (setf aux 0) T)
					(T (setf aux 0) NIL)))))

; cria-pred-9(lista) - Create Closure used in positions with restriction equal to 9.				
(defun cria-pred-9 (lista)
	(let ((aux -1) (aux2 NIL) (lis lista))
		#'(lambda (psr)
			(dolist (ele lis NIL)
					(when (null (psr-variavel-valor psr ele)) (setf aux T)  (return))
					(when (equal 0 (psr-variavel-valor psr ele)) (setf aux NIL) (return)))
					 (cond ((equal aux -1) T) 
							(T (setf aux2 aux) (setf aux -1) aux2)))))	
							
; cria-pred-0(lista) - Create Closure used in positions with restriction equal to 0.							
(defun cria-pred-0 (lista)
	(let ((aux -1) (aux2 NIL) (lis lista))
		#'(lambda (psr)
			(dolist (ele lis NIL)
					(when (null (psr-variavel-valor psr ele)) (setf aux T) (return))
					(when (equal 1 (psr-variavel-valor psr ele)) (setf aux NIL) (return)))
					(cond ((equal aux -1) T) 
							(T (setf aux2 aux) (setf aux -1) aux2)))))

							
; fill-a-pix->psr(array) - Transforms a Fill-a-Pix array-problem in a PSR.
(defun fill-a-pix->psr (array)
  (let*(
	(i 0)
	(val NIL)
	(dom (list 0 1))
	(restList '())
	(nlinhas (first(array-dimensions array)))             
	(ncolunas (second(array-dimensions array)))    
	(domList (make-list (* nlinhas ncolunas) :initial-element (copy-list dom)))
	(varlist (make-list (* nlinhas ncolunas) :initial-element (list -1 -1))))        	   
	(dotimes (a nlinhas)   
	(dotimes (b ncolunas)      
	(setf (nth i varList) (format nil "~D ~D" b a))
	(setf i (+ i 1)))) 
	(dotimes (y ncolunas)   
		(dotimes (x nlinhas)
			(setf val (aref array x y))
			(cond ((equal val 9)
				(setf restList(append restList (list   
					(cria-restricao (boarders x y nlinhas ncolunas) 
					(cria-pred-9 (boarders x y  nlinhas ncolunas))
					)))))
				((equal val 0)
				(setf restList(append restList (list   
					(cria-restricao (boarders x y nlinhas ncolunas) 
					(cria-pred-0 (boarders x y nlinhas ncolunas))
					)))))
				((not (null val))
				(setf restList(append restList (list   
					(cria-restricao (boarders x y nlinhas ncolunas) 
					(cria-pred-geral val (boarders x y nlinhas ncolunas))
					))))))))
	(cria-psr varList domList restList)))

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
;#############################################################################
;######################### FUNCOES PARA RESOLUCAO CSP ########################

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! PSEUDO-CODE !!!!!!!!!!!!!!!!!!!!!!!!!!
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

; procura-retrocesso-simples(psr) - Receives a PSR and search for a solution using only BackTracking Search.
(defun procura-retrocesso-simples(psr)
	(let ((numTests 0) (aux1 0) (res NIL) (res1 NIL) (test NIL) (var (first (psr-variaveis-nao-atribuidas psr))))
		(cond ((psr-completo-p psr) 
			(return-from procura-retrocesso-simples (values psr numTests))))
		(dolist (atr (psr-variavel-dominio psr var) NIL)
			(setf res1 (multiple-value-list (psr-atribuicao-consistente-p psr var atr)))
			(setf test (nth 0 res1))
			(setf aux1 (nth 1 res1))
			(setf numTests (+ numTests aux1))
			(cond ((equal test T)
				(psr-adiciona-atribuicao! psr var atr)
				(setf res1 (multiple-value-list (procura-retrocesso-simples psr)))
				(setf res (nth 0 res1))
				(setf aux1 (nth 1 res1))
				(setf numTests (+ numTests aux1))
				(cond ((not (equal res FAILURE)) (return-from procura-retrocesso-simples (values res numTests))))
				(psr-remove-atribuicao! psr var))))
	(values FAILURE numTests)))

; resolve-simples(array) - Receives a Fill-a-Pix (array) and try solve it.
(defun resolve-simples(arr)
	(let ((res (procura-retrocesso-simples (fill-a-pix->psr arr))))
		(cond ((equal res FAILURE)
			NIL)
			(T (psr->fill-a-pix res (array-dimension arr 0) (array-dimension arr 1))))))

;################################################################################################
;################################ FUNCOES PARTE 2 PROJECTO ######################################
;################################################################################################			

; maximum-degree(psr) - Returns the maximum degree variable.
(defun maximum-degree(psr)
	(let ((varList (psr-variaveis-nao-atribuidas psr)) (maximumVar NIL) (aux 0) (maximumNum -1))
		(dolist (var varList NIL)
			(dolist (restr (psr-variavel-restricoes psr var) NIL)
				(dolist (ele (restricao-variaveis restr) NIL)
					(cond ((and (not (equal var ele)) (membro ele (psr-variaveis-nao-atribuidas psr)))
						(setf aux (+ aux 1)) (return)))))
			(cond ((< maximumNum aux)
				(setf maximumNum aux) (setf maximumVar var)))
			(setf aux 0))
	maximumVar))

	
; procura-retrocesso-grau(psr) - Backtracking Search using Maximum Degree Heuristic.
(defun procura-retrocesso-grau(psr)
	(let ((numTests 0) (aux1 0) (res NIL) (res1 NIL) (test NIL) (var (maximum-degree psr)))
		(cond ((psr-completo-p psr) 
			(return-from procura-retrocesso-grau (values psr numTests))))
		(dolist (atr (psr-variavel-dominio psr var) NIL)
			(setf res1 (multiple-value-list (psr-atribuicao-consistente-p psr var atr)))
			(setf test (nth 0 res1))
			(setf aux1 (nth 1 res1))
			(setf numTests (+ numTests aux1))
			(cond ((equal test T)
				(psr-adiciona-atribuicao! psr var atr)
				(setf res1 (multiple-value-list (procura-retrocesso-grau psr)))
				(setf res (nth 0 res1))
				(setf aux1 (nth 1 res1))
				(setf numTests (+ numTests aux1))
				(cond ((not (equal res FAILURE)) (return-from procura-retrocesso-grau (values res numTests))))
				(psr-remove-atribuicao! psr var))))
	(values FAILURE numTests)))

;==========================================================================================	

; FUNCOES E ESTRUTURAS AUXILIARES
; IMPORTANTE: A lista da inferencia e uma lista de tuplos (var . dominio) em que dominio e uma lista de valores.
(defstruct inferencia (lista NIL))

; MRV(psr) - Returns MRV (Minimum Remaining Value) variable.
(defun MRV(psr)
	(let ((varList (psr-variaveis-nao-atribuidas psr)) (select-var NIL) (minimum-domain-size NIL) (aux 0))
		(setf select-var (first varList))
		(setf minimum-domain-size (length (psr-variavel-dominio psr select-var)))
		(setf varList (rest varList))
		(dolist (ele varList NIL)
			(setf aux (length (psr-variavel-dominio psr ele)))
			(cond ((< aux minimum-domain-size)
					(setf select-var ele)
					(setf minimum-domain-size aux))))
	select-var))

; adiciona-inferencias(psr inferencias) - 
(defun 	adiciona-inferencias(psr inferencias)
	(let ((lista (inferencia-lista inferencias)) (dom NIL))
		(dolist (ele lista NIL)
			(setf dom (psr-variavel-dominio psr (car ele)))
			(psr-altera-dominio! psr (car ele) (cdr ele))
			(setf (cdr ele) dom))))
	
; get-dominio-inferencias(var inferencias) -
(defun get-dominio-inferencias(var inferencias)
	(let ((lista (inferencia-lista inferencias)))
		(dolist (ele lista NIL)
			(cond ((equal var (car ele))
					(return-from get-dominio-inferencias (cdr ele)))))
		NIL))
	
; set-dominio-inferencias(var inferencias) -
(defun set-dominio-inferencias(var dominio inferencias)
	(let ((lista (inferencia-lista inferencias)))
		(dolist (ele lista NIL)
				(cond ((equal var (car ele))
						(setf (cdr ele) dominio)
						(return-from set-dominio-inferencias))))
		(setf (inferencia-lista inferencias) (cons (cons var dominio) (inferencia-lista inferencias)))))
	
; revise(psr x y inferencias) - 
(defun revise(psr x y inferencias)
	(let ((testesTotais 0) (revised NIL) (dominio-x NIL) (dominio-y NIL) (novo-dominio-x NIL) 
	(foundConsistentValue NIL) (aux NIL))
		(setf aux (get-dominio-inferencias x inferencias))
		(if aux (setf dominio-x aux)
				  (setf dominio-x (copy-list (psr-variavel-dominio psr x))))	;Faz copia do que esta no PSR.
		(setf novo-dominio-x dominio-x)
		(setf aux (get-dominio-inferencias y inferencias))
		(if (not (membro y (psr-variaveis-nao-atribuidas psr))) (setf dominio-y (list (psr-variavel-valor psr y)))
			(if aux (setf dominio-y aux)
						(setf dominio-y (copy-list (psr-variavel-dominio psr y)))))
			
		(dolist (vx dominio-x NIL)
			(setf foundConsistentValue NIL)
			(dolist (vy dominio-y NIL)
				(setf aux (multiple-value-list (psr-atribuicoes-consistentes-arco-p psr x vx y vy)))
				(setf testesTotais (+ testesTotais (nth 1 aux)))
				(cond ((nth 0 aux)
					(setf foundConsistentValue T) (return))))
			(cond ((not foundConsistentValue)
				(setf revised T)
				(setf novo-dominio-x (remove vx novo-dominio-x)))))
		(cond (revised
			(set-dominio-inferencias x novo-dominio-x inferencias)))
	(values revised testesTotais)))

; arcos-vizinhos-nao-atribuidos(psr var) -
(defun arcos-vizinhos-nao-atribuidos(psr var)
	(let ((result NIL) (var-restr (psr-variavel-restricoes psr var)) (vars-nao-atribuidas (psr-variaveis-nao-atribuidas psr)) (aux NIL))
		(dolist (restr var-restr NIL)
			(setf aux (restricao-variaveis restr))
			(dolist (ele aux NIL)
				(if (not (equal var ele))
					(cond ((and (membro ele vars-nao-atribuidas) (not (membro (cons ele var) result)))
						(setf result (cons (cons ele var) result)))))))
	(reverse result)))
						
; forward-checking(psr var) - 
(defun forward-checking(psr var)
	(let ((inferencias (make-inferencia)) (testesTotais 0) (lista-arcos (arcos-vizinhos-nao-atribuidos psr var)) (aux NIL))
		(dolist (ele lista-arcos NIL)
			(setf aux (multiple-value-list (revise psr (car ele) (cdr ele) inferencias)))
			(setf testesTotais (+ testesTotais (nth 1 aux)))
			(cond ((nth 0 aux)
					(if (equal (length (get-dominio-inferencias (car ele) inferencias)) 0)
						(return-from forward-checking (values NIL testesTotais))))))
	(values inferencias testesTotais)))
		
; procura-retocesso-fc-mrv(psr) - Backtracking Search using Forward Checking mechanism 
; and MRV (Minimum Remaining Value) Heuristic.
(defun procura-retrocesso-fc-mrv(psr)
	(let ((testesTotais 0) (aux1 0) (dom NIL) (res NIL) (res1 NIL) (test NIL) (var (MRV psr)) (inf NIL))
		(cond ((psr-completo-p psr) 
			(return-from procura-retrocesso-fc-mrv (values psr testesTotais))))
			
		(setf dom (psr-variavel-dominio psr var))
		(dolist (atr dom NIL)
			(setf res1 (multiple-value-list (psr-atribuicao-consistente-p psr var atr)))
			(setf test (nth 0 res1))
			(setf aux1 (nth 1 res1))
			(setf testesTotais (+ testesTotais aux1))
			(cond ((equal test T)
				(psr-adiciona-atribuicao! psr var atr)
				(setf res1 (multiple-value-list (forward-checking psr var)))
				(setf testesTotais (+ testesTotais (nth 1 res1)))
				(setf inf (nth 0 res1))
				(cond (inf
					(adiciona-inferencias psr (nth 0 res1))
					(setf res1 (multiple-value-list (procura-retrocesso-fc-mrv psr)))
					(setf res (nth 0 res1))
					(setf aux1 (nth 1 res1))
					(setf testesTotais (+ testesTotais aux1))
					(cond ((not (equal res FAILURE)) 
						(return-from procura-retrocesso-fc-mrv (values res testesTotais))))
					(adiciona-inferencias psr inf)))
				(psr-remove-atribuicao! psr var))))
	(values FAILURE testesTotais)))

;==========================================================================================	


; procura-retrocesso-mac-mrv(psr) - Solves CSP using MAC (Maintain Arc Consistency) mechanism and MRV
; heuristic.
(defun procura-retrocesso-mac-mrv(psr)
psr)



;==========================================================================================



; resolve-best(array) - Receives and Fill-a-Pix array and use best algorythm to solve it.
(defun resolve-best(array)
array)
	
			
;========================= FIM FUNCOES PARA RESOLUCAO CSP =================================================

(defvar puzzle1)
(defvar puzzle2)
(defvar puzzle5)
(defvar puzzle1.1)
(defvar psr5)
(defvar psr2)
(defvar psr1)
(defvar psr1.1)

(setf puzzle1 (make-array (list 5 5) :initial-contents
	'((NIL NIL 1 NIL NIL)
	  (NIL 1 NIL NIL 5)
	  (1 NIL NIL NIL 6)
	  (NIL NIL NIL 9 NIL)
	  (NIL 5 6 NIL NIL))))

(setf puzzle1.1 (make-array (list 5 5) :initial-contents
	'((NIL 2 3 NIL NIL)
	  (NIL NIL NIL NIL NIL)
	  (NIL NIL 5 NIL NIL)
	  (NIL 4 NIL 5 NIL)
	  (NIL NIL 4 NIL NIL)
	  )))
	  
(setf puzzle2 (make-array (list 10 10) :initial-contents
	'((NIL 2 3 NIL NIL 0 NIL NIL NIL NIL)
	  (NIL NIL NIL NIL 3 NIL 2 NIL NIL 6)
	  (NIL NIL 5 NIL 5 3 NIL 5 7 4)
	  (NIL 4 NIL 5 NIL 5 NIL 6 NIL 3)
	  (NIL NIL 4 NIL 5 NIL 6 NIL NIL 3)
	  (NIL NIL NIL 2 NIL 5 NIL NIL NIL NIL)
	  (4 NIL 1 NIL NIL NIL 1 1 NIL NIL)
	  (4 NIL 1 NIL NIL NIL 1 NIL 4 NIL)
	  (NIL NIL NIL NIL 6 NIL NIL NIL NIL 4)
	  (NIL 4 4 NIL NIL NIL NIL 4 NIL NIL))))

(setf puzzle5 (make-array (list 15 15) :initial-contents
	'((0 NIL NIL 4 3 2 1 NIL NIL NIL NIL NIL 3 NIL NIL)
	  (NIL NIL 5 NIL NIL 4 NIL NIL 4 4 NIL NIL NIL NIL 3)
	  (NIL 5 4 5 4 5 5 NIL 5 3 NIL 1 2 NIL 3)
	  (4 NIL NIL NIL 4 NIL NIL 4 2 NIL 1 NIL NIL NIL NIL)
	  (NIL NIL 5 4 NIL 2 2 NIL 1 0 NIL NIL 7 5 NIL)
	  (NIL NIL NIL 5 NIL NIL 0 NIL NIL NIL NIL 4 5 NIL 2)
	  (4 NIL NIL 5 4 2 0 0 NIL NIL NIL 5 6 NIL NIL)
	  (5 NIL NIL 6 5 NIL NIL NIL NIL NIL 3 3 3 NIL 3)
	  (NIL NIL 5 NIL 5 3 NIL NIL NIL NIL NIL NIL 3 NIL NIL)
	  (5 NIL NIL 6 5 NIL 3 5 NIL 6 NIL NIL 0 NIL 0)
	  (NIL NIL 5 NIL 4 3 2 4 5 NIL 4 NIL NIL 1 NIL)
	  (NIL 7 NIL NIL 5 NIL NIL 1 NIL 5 5 5 NIL NIL NIL)
	  (NIL NIL 6 4 4 4 3 1 2 4 NIL NIL 6 4 NIL)
	  (NIL 5 NIL 6 NIL NIL NIL NIL NIL 4 6 NIL NIL NIL NIL)
	  (NIL NIL NIL NIL NIL NIL 3 2 0 NIL 4 4 3 NIL 2))))

(setf psr1.1 (fill-a-pix->psr puzzle1.1))
(setf psr2 (fill-a-pix->psr puzzle2))
(setf psr5 (fill-a-pix->psr puzzle5))
(setf psr1 (fill-a-pix->psr puzzle1))	  