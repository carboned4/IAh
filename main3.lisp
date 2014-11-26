;Projecto IA
;Grupo:
;Artur Jose Fonseca					N 75456 
;Andre Filipe Pardal Pires			N 76046
;Miguel de Oliveira Melicia Martins N 76102

(load "exemplos.fas")

;=========================== FUNCOES AUXILIARES ============================
; membro(elemento lista) - Verifies if the element is in the list.
(defun membro (ele lis)
	(cond ((null lis) nil)
		((equal ele (first lis)) T)
		(T (membro ele (rest lis)))))
		
;========================== ESTRUTURAS DE DADOS ===========================

; 						TIPO RESTRICAO
(defstruct restricao (lista-var nil) (predicado nil))

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
(defstruct var (nome nil) (valor nil) (dom nil))

(defstruct psr (variaveis-hash nil) (lista-var nil) (lista-restr nil))

;###Constructor###
; cria-psr(lista lista lista) - Create PSR.	
(defun cria-psr (lista-v lista-d lista-r)
	(let ((vars-hash (make-hash-table :test #'equal :size (length lista-v) :rehash-threshold 1.0)) (iter-var lista-v) (iter-dom lista-d))
		(loop do
			(setf (gethash (first iter-var) vars-hash) (make-var :nome (first iter-var) :dom (first iter-dom)))
			(setf iter-var (rest iter-var))
			(setf iter-dom (rest iter-dom))
		while(not(null iter-var)))
		(let ((psr (make-psr :variaveis-hash vars-hash :lista-restr lista-r :lista-var lista-v)))
			psr)))		
;#################			
					
;###Functions###					
; psr-atribuicoes(psr) - Returns a list with all "PAIRS" (var . value) of PSR.
(defun psr-atribuicoes (psr)
	(let ((res nil) (iter-var (psr-lista-var psr)))
		(loop do
			(when (not(equal (var-valor (gethash (first iter-var) (psr-variaveis-hash psr))) nil))
				(setf res (append res (list(cons (first iter-var) (var-valor (gethash (first iter-var) (psr-variaveis-hash psr))))))))
			(setf iter-var (rest iter-var))
			while(not(null iter-var)))
	res))

; psr-variaveis-todas(psr) - Returns a list with all variables.		 
(defun psr-variaveis-todas (psr)
	(psr-lista-var psr))
		
; psr-variaveis-nao-atribuidas(psr) - Returns list with all variables that have
; no value.
(defun psr-variaveis-nao-atribuidas (psr)
	(let ((res nil) (iter-var (psr-lista-var psr)))
		(loop do
			(when (equal (var-valor (gethash (first iter-var) (psr-variaveis-hash psr))) nil)
				(setf res (append res (list  (first iter-var)))))
			(setf iter-var (rest iter-var))
		while(not(null iter-var)))
	res))

; psr-variavel-valor(psr variavel) - Function returns value of variable or nil if variable
; doesnt have one.
(defun psr-variavel-valor(psr var)
	(var-valor (gethash var (psr-variaveis-hash psr))))
				
; psr-variavel-dominio(psr var) - Returns var domain.
(defun psr-variavel-dominio (psr var)
	(var-dom (gethash var (psr-variaveis-hash psr))))
	
; psr-variavel-restricoes(psr var) - Returns all restriction applied to var in the psr.
(defun psr-variavel-restricoes(psr var)
	(cond ((equal (psr-lista-restr psr) nil) (return-from psr-variavel-restricoes nil)))
	(let ((res nil) (i (psr-lista-restr psr)))
		(loop do
			(when (membro var (restricao-variaveis (first i)))
				(setf res (append res (list (first i)))))
			(setf i (rest i))
		while(not(null i)))
	res))

; psr-adiciona-atribuicao! (psr var valor) - Adds a value to the var.
(defun psr-adiciona-atribuicao! (psr var valor)
	(setf (var-valor (gethash var (psr-variaveis-hash psr))) valor))
		
;psr-remove-atribuicao!(psr var) - Makes the var without VALUE.  
(defun psr-remove-atribuicao! (psr var)
	(setf (var-valor (gethash var (psr-variaveis-hash psr))) nil))
	
; psr-altera-dominio!(psr var dom) - Changes var Domain.	
(defun psr-altera-dominio! (psr var dom)
	(setf (var-dom (gethash var (psr-variaveis-hash psr))) dom))	

; psr-completo-p(psr) - Verifies if all variables have a value.
(defun psr-completo-p(psr)
	(cond ((null (psr-variaveis-nao-atribuidas psr)) 
			T)
		(T 
			nil)))

; psr-consistente-p(psr) - Verifies if the CSP is consistent (VERIFIES ALL CSP RESTRICTIONS).
(defun psr-consistente-p(psr)
	(cond ((equal (psr-lista-restr psr) nil) (return-from psr-consistente-p (values T 0))))
	(let ((count 0) (restr (psr-lista-restr psr)))								;All restrictions.
		(dolist (restricao restr)
			(incf count)
			(when (not(funcall (restricao-funcao-validacao restricao) psr))		;Call to restriction predicate.
				(return-from psr-consistente-p (values nil count))))
	(values T count)))

; psr-variavel-consistente-p(psr var) - Verifies all variable restrictions.
(defun psr-variavel-consistente-p (psr var)
	(let ((count 0) (restr (psr-variavel-restricoes psr var)))				;Restriction affects variable.
		(cond ((null restr) (return-from psr-variavel-consistente-p (values T 0))))
		(dolist (ele restr)
			(incf count)
			(when (not(funcall (restricao-funcao-validacao ele) psr))		;Call to restriction predicate.
				(return-from psr-variavel-consistente-p (values nil count))))
		(values T count)))

; psr-atribuicao-consistente-p(psr var value) - Verifies if { var = value } maintain var restricitons consistent.
(defun psr-atribuicao-consistente-p(psr var valor)
	(cond ((equal (psr-lista-restr psr) nil) (return-from psr-atribuicao-consistente-p (values T 0))))
	(let ((res nil) (aux (psr-variavel-valor psr var)))
		(cond ((equal aux nil) (setf aux nil)))
		(psr-adiciona-atribuicao! psr var valor)
		(setf res (multiple-value-list (psr-variavel-consistente-p psr var)))
		(psr-adiciona-atribuicao! psr var aux)
		(return-from psr-atribuicao-consistente-p (values (nth 0 res) (nth 1 res)))))
		
; psr-atribuicoes-consistentes-arco-p(psr var1 v1 var2 v2) - Verifies if 2 variables are consistent in arc.
(defun psr-atribuicoes-consistentes-arco-p (psr var1 v1 var2 v2)
	(cond ((equal (psr-lista-restr psr) nil) (return-from psr-atribuicoes-consistentes-arco-p (values T 0))))
	(let ((testes 0) (aux1 (psr-variavel-valor psr var1)) (aux2 (psr-variavel-valor psr var2)))
		(cond ((equal aux1 nil) (setf aux1 nil)))
		(cond ((equal aux2 nil) (setf aux2 nil)))
		(psr-adiciona-atribuicao! psr var1 v1)
		(psr-adiciona-atribuicao! psr var2 v2)
		(dolist (r (psr-variavel-restricoes psr var1))
			(cond ((membro var2 (restricao-variaveis r))
					(incf testes)
					(cond ((not (funcall (restricao-funcao-validacao r) psr))
						(psr-adiciona-atribuicao! psr var1 aux1)
						(psr-adiciona-atribuicao! psr var2 aux2)
						(return-from psr-atribuicoes-consistentes-arco-p (values nil testes)))))))
		(psr-adiciona-atribuicao! psr var1 aux1)
		(psr-adiciona-atribuicao! psr var2 aux2)
		(values T testes)))
				
;========================= FIM ESTRUTURAS DE DADOS ===============================

;=================================================================================

;========================= FUNCOES DO TABULEIRO ==================================

; AUXILIAR FUNCTIONS

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
	(let ((aux 0) (nils 0) (cmp valor) (lis lista))
		#'(lambda (psr)
			(dolist (ele lis)
				(when (null (psr-variavel-valor psr ele)) (incf nils))
				(when (psr-variavel-valor psr ele) (setf aux (+ (psr-variavel-valor psr ele) aux))))
			(cond ((and (>= (+ nils aux) cmp) (not (> aux cmp))) (setf aux 0) (setf nils 0) T)
				(T (setf aux 0) (setf nils 0) nil)))))

; cria-pred-9(lista) - Create Closure used in positions with restriction equal to 9.				
(defun cria-pred-9 (lista)
	(let ((aux -1) (aux2 nil) (lis lista))
		#'(lambda (psr)
			(dolist (ele lis)
				(when (equal 0 (psr-variavel-valor psr ele)) (setf aux nil) (return)))
			 (cond ((equal aux -1) T) 
				(T (setf aux2 aux) (setf aux -1) aux2)))))	

							
; cria-pred-0(lista) - Create Closure used in positions with restriction equal to 0.							
(defun cria-pred-0 (lista)
	(let ((aux -1) (aux2 nil) (lis lista))
		#'(lambda (psr)
			(dolist (ele lis)
				(when (equal 1 (psr-variavel-valor psr ele)) (setf aux nil) (return)))
			(cond ((equal aux -1) T) 
				(T (setf aux2 aux) (setf aux -1) aux2)))))

							
; fill-a-pix->psr(array) - Transforms a Fill-a-Pix array-problem in a PSR.
(defun fill-a-pix->psr (array)
  (let*(
	(i 0)
	(val nil)
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
			(cond ((and (equal val 6) (equal (length (boarders x y  nlinhas ncolunas)) 6))
					(setf restList(append restList (list   
					(cria-restricao (boarders x y nlinhas ncolunas) 
					(cria-pred-9 (boarders x y  nlinhas ncolunas))
					)))))
				((and (equal val 4) (equal (length (boarders x y  nlinhas ncolunas)) 4))
					(setf restList(append restList (list   
					(cria-restricao (boarders x y nlinhas ncolunas) 
					(cria-pred-9 (boarders x y  nlinhas ncolunas))
					)))))
				((equal val 9)
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
	(let ((res (make-array (list int1 int2))) (atribuicoes (psr-atribuicoes psr)) (aux nil))
		(dolist (atr atribuicoes)
			(setf aux (with-input-from-string (in (car atr)) (loop for x = (read in nil nil) while x collect x)))
				(setf (aref res (nth 0 aux) (nth 1 aux)) (cdr atr)))
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

; procura-retrocesso-simples(psr) - Receives a PSR and search for a solution using only BackTracking Search
; without heuristics or restriciton propagation mechanism.
(defun procura-retrocesso-simples(psr)
	(let ((testesTotais 0) (res nil) (var (first (psr-variaveis-nao-atribuidas psr))))
		(cond ((psr-completo-p psr) 
			(return-from procura-retrocesso-simples (values psr testesTotais))))
		(dolist (atr (psr-variavel-dominio psr var))
			(setf res (multiple-value-list (psr-atribuicao-consistente-p psr var atr)))
			(setf testesTotais (+ testesTotais (nth 1 res)))
			(cond ((nth 0 res)
				(psr-adiciona-atribuicao! psr var atr)
				(setf res (multiple-value-list (procura-retrocesso-simples psr)))
				(setf testesTotais (+ testesTotais(nth 1 res)))
				(cond ((nth 0 res) (return-from procura-retrocesso-simples (values (nth 0 res) testesTotais))))
				(psr-remove-atribuicao! psr var))))
	(values nil testesTotais)))

; resolve-simples(array) - Receives a Fill-a-Pix (array) and try solve it.
(defun resolve-simples(arr)
	(let ((res (procura-retrocesso-simples (fill-a-pix->psr arr))))
		(cond ((equal res nil)
			nil)
			(T (psr->fill-a-pix res (array-dimension arr 0) (array-dimension arr 1))))))

;################################################################################################
;################################ FUNCOES PARTE 2 PROJECTO ######################################
;################################################################################################			

; maximum-degree(psr) - Returns the maximum degree variable.
(defun maximum-degree(psr)
	(let ((varList  (psr-variaveis-nao-atribuidas psr)) (maximumVar nil) (aux 0) (maximumNum -1))
		(dolist (var varList)
			(setf aux 0)
			(dolist (restr (psr-variavel-restricoes psr var))
				(dolist (ele (restricao-variaveis restr))
					(cond ((and (not (equal var ele)) (equal nil (psr-variavel-valor psr ele)))
						(incf aux) (return)))))
			(cond ((< maximumNum aux)
				(setf maximumNum aux) (setf maximumVar var))))
	maximumVar))

; procura-retrocesso-grau(psr) - Backtracking Search using Maximum Degree Heuristic.
(defun procura-retrocesso-grau(psr)
	(let ((testesTotais 0) (res nil) (var nil))
		(cond ((psr-completo-p psr) 
			(return-from procura-retrocesso-grau (values psr testesTotais))))
		(setf var (maximum-degree psr))
		(dolist (atr (psr-variavel-dominio psr var))
			(setf res (multiple-value-list (psr-atribuicao-consistente-p psr var atr)))
			(setf testesTotais (+ testesTotais (nth 1 res)))
			(cond ((nth 0 res)
				(psr-adiciona-atribuicao! psr var atr)
				(setf res (multiple-value-list (procura-retrocesso-grau psr)))
				(setf testesTotais (+ testesTotais (nth 1 res)))
				(cond ((not (equal (nth 0 res) nil)) (return-from procura-retrocesso-grau (values (nth 0 res) testesTotais))))
				(psr-remove-atribuicao! psr var))))
	(values nil testesTotais)))

;==========================================================================================	

; AXILIAR FUNCTIONS AND STRUCTURES
; IMPORTANTE: A lista da inferencia e uma lista de tuplos (var . dominio) em que dominio e uma lista de valores.
(defstruct inferencia (lista nil))

; MRV(psr) - Returns MRV (Minimum Remaining Value) variable.
(defun MRV(psr)
	(let ((varList (psr-variaveis-nao-atribuidas psr)) (select-var nil) (minimum-domain-size nil) (aux 0))
		(setf select-var (first varList))
		(setf minimum-domain-size (length (psr-variavel-dominio psr select-var)))
		(setf varList (rest varList))
		(dolist (ele varList)
			(setf aux (length (psr-variavel-dominio psr ele)))
			(cond ((< aux minimum-domain-size)
					(setf select-var ele)
					(setf minimum-domain-size aux))))
	select-var))

; adiciona-inferencias(psr inferencias) - Add new dom and saves the old ones in inferencias.
(defun adiciona-inferencias(psr inferencias)
	(let ((lista (inferencia-lista inferencias)) (dom nil))
		(dolist (ele lista)
			(setf dom (psr-variavel-dominio psr (car ele)))
			(psr-altera-dominio! psr (car ele) (cdr ele))
			(setf (cdr ele) dom))))
	
; get-dominio-inferencias(var inferencias) - Get var's dom saved in inferencias.
(defun get-dominio-inferencias(var inferencias)
	(let ((lista (inferencia-lista inferencias)))
		(dolist (ele lista)
			(cond ((equal var (car ele))
					(return-from get-dominio-inferencias (cdr ele)))))
		-1))
	
; set-dominio-inferencias(var inferencias) - Set var's dom saved in inferencias.
(defun set-dominio-inferencias(var dominio inferencias)
	(let ((lista (inferencia-lista inferencias)))
		(dolist (ele lista)
				(cond ((equal var (car ele))
						(setf (cdr ele) dominio)
						(return-from set-dominio-inferencias))))			;UPDATE dom
		(setf (inferencia-lista inferencias) (cons (cons var dominio) (inferencia-lista inferencias)))));ADD dom
		
; revise(psr x y inferencias) - Tries make x and y consistent in arc.
(defun revise(psr x y inferencias)
	(let ((testesTotais 0) (revised nil) (dominio-x nil) (dominio-y nil) (novo-dominio-x nil) 
	(foundConsistentValue nil) (aux nil))
		(setf aux (get-dominio-inferencias x inferencias))
		(if (not (equal aux -1)) (setf dominio-x aux)
				  (setf dominio-x (copy-list (psr-variavel-dominio psr x))))	;Faz copia do que esta no PSR.
		(setf novo-dominio-x dominio-x)
		(setf aux (get-dominio-inferencias y inferencias))
		(if (psr-variavel-valor psr y) (setf dominio-y (list (psr-variavel-valor psr y)))
			(if (not (equal aux -1)) (setf dominio-y aux)
						(setf dominio-y (copy-list (psr-variavel-dominio psr y)))))
			
		(dolist (vx dominio-x)
			(setf foundConsistentValue nil)
			(dolist (vy dominio-y)
				(setf aux (multiple-value-list (psr-atribuicoes-consistentes-arco-p psr x vx y vy)))
				(setf testesTotais (+ testesTotais (nth 1 aux)))
				(cond ((nth 0 aux)
					(setf foundConsistentValue T) (return))))
			(cond ((not foundConsistentValue)
				(setf revised T)
				(setf novo-dominio-x (remove vx novo-dominio-x :test #'equal)))))
		(cond (revised
			(set-dominio-inferencias x novo-dominio-x inferencias)))
	(values revised testesTotais)))

; arcos-vizinhos-nao-atribuidos(psr var) - 
(defun arcos-vizinhos-nao-atribuidos(psr var)
	(let ((result nil))
		(dolist (var-natribuida (psr-variaveis-nao-atribuidas psr))
			(cond ((not (equal var var-natribuida))
				(dolist (ele (psr-variavel-restricoes psr var))
					(cond ((and (membro var-natribuida (restricao-variaveis ele)) (not(membro (cons var-natribuida var) result)))
						(setf result (append result (list (cons var-natribuida var))))))))))
		result))
						
; forward-checking(psr var) - Mechanism used in restriction propagation.
(defun forward-checking(psr var)
	(let ((inferencias (make-inferencia)) (testesTotais 0) (lista-arcos (arcos-vizinhos-nao-atribuidos psr var)) (aux nil))
		(dolist (arco lista-arcos)
			(setf aux (multiple-value-list (revise psr (car arco) (cdr arco) inferencias)))
			(setf testesTotais (+ testesTotais (nth 1 aux)))
			(cond ((nth 0 aux)
					;Found Variable that have no possible value left and returns nil.
					(if (equal (length (get-dominio-inferencias (car arco) inferencias)) 0)	
						(return-from forward-checking (values nil testesTotais))))))
	(values inferencias testesTotais)))
		
; procura-retocesso-fc-mrv(psr) - Backtracking Search using Forward Checking mechanism 
; and MRV (Minimum Remaining Value) Heuristic.
(defun procura-retrocesso-fc-mrv(psr)
	(let ((testesTotais 0) (res nil) (res1 nil) (var nil) (inf nil))
		(cond ((psr-completo-p psr) 
			(return-from procura-retrocesso-fc-mrv (values psr testesTotais))))
		(setf var (MRV psr))
		(dolist (atr (psr-variavel-dominio psr var))
			(setf res1 (multiple-value-list (psr-atribuicao-consistente-p psr var atr)))
			(setf testesTotais (+ testesTotais (nth 1 res1)))
			(cond ((nth 0 res1)
				(psr-adiciona-atribuicao! psr var atr)
				(setf res1 (multiple-value-list (forward-checking psr var)))
			
				(setf testesTotais (+ testesTotais (nth 1 res1)))
				(setf inf (nth 0 res1))
				(cond (inf
					(adiciona-inferencias psr inf)
					(setf res1 (multiple-value-list (procura-retrocesso-fc-mrv psr)))
					(setf res (nth 0 res1))
					(setf testesTotais (+ testesTotais (nth 1 res1)))
					(cond ((not (equal res nil)) 
						(return-from procura-retrocesso-fc-mrv (values res testesTotais))))
					(adiciona-inferencias psr inf)))
				(psr-remove-atribuicao! psr var))))
	(values nil testesTotais)))

;=========================================================================================	

; aux(psr lista infrenecia) - Auxiliar function used do tierate arc-list while being expanded.
(defun mac-list(psr lista inferencia)
	(let ((testesTotais 0) (aux nil) (inferencias inferencia) (lista-arcos lista) (return-arcos nil)(novos-arcos nil))
		(dolist (arco lista-arcos)
			(setf aux (multiple-value-list (revise psr (car arco) (cdr arco) inferencias)))
			(setf testesTotais (+ testesTotais (nth 1 aux)))
			(cond ((nth 0 aux)
				(if (equal (length (get-dominio-inferencias (car arco) inferencias)) 0)
					(return-from mac-list (values nil testesTotais return-arcos inferencia)))
				(setf novos-arcos (arcos-vizinhos-nao-atribuidos psr (car arco)))
				(setf novos-arcos (remove (cons (cdr arco) (car arco)) novos-arcos :test #'equal))
				(setf return-arcos (append return-arcos novos-arcos)))))
		(values T testesTotais return-arcos inferencia)))
				
				
; MAC(psr var) - Restriction propagation mechanism.
(defun MAC(psr var)
	(let ((testesTotais 0) (inferencias (make-inferencia)) (lista-arcos (arcos-vizinhos-nao-atribuidos psr var))
		(aux nil) (repeat nil))
		(loop do
			(setf aux (multiple-value-list (mac-list psr lista-arcos inferencias)))
			(setf repeat(nth 0 aux))
			(setf testesTotais (+ testesTotais (nth 1 aux)))
			(setf lista-arcos (nth 2 aux))
			(setf inferencias (nth 3 aux))
			(if (not repeat)
				(return-from MAC (values nil testesTotais)))
		while(not (null lista-arcos)))
		(values inferencias testesTotais)))
					
; procura-retrocesso-mac-mrv(psr) - Solves CSP using MAC (Maintain Arc Consistency) mechanism and MRV
; heuristic.
(defun procura-retrocesso-mac-mrv(psr)
	(let ((testesTotais 0) (res nil) (res1 nil) (var nil) (inf nil))
		(cond ((psr-completo-p psr) 
			(return-from procura-retrocesso-mac-mrv (values psr testesTotais))))
		(setf var (MRV psr))	
		(dolist (atr (psr-variavel-dominio psr var))
			(setf res1 (multiple-value-list (psr-atribuicao-consistente-p psr var atr)))
			(setf testesTotais (+ testesTotais (nth 1 res1)))			
			(cond ((nth 0 res1)
				(psr-adiciona-atribuicao! psr var atr)
				(setf res1 (multiple-value-list (MAC psr var)))
				(setf testesTotais (+ testesTotais (nth 1 res1)))
				(setf inf (nth 0 res1))
				(cond (inf
					(adiciona-inferencias psr inf)
					(setf res1 (multiple-value-list (procura-retrocesso-mac-mrv psr)))
					(setf res (nth 0 res1))
					(setf testesTotais (+ testesTotais (nth 1 res1)))
					(cond ((not (equal res nil)) 
						(return-from procura-retrocesso-mac-mrv (values res testesTotais))))
					(adiciona-inferencias psr inf)))
				(psr-remove-atribuicao! psr var))))
	(values nil testesTotais)))

;==========================================================================================

; resolve-best(array) - Receives and Fill-a-Pix array and use best algorythm to solve it.
(defun resolve-best(arr)
  (let ((res (procura-retrocesso-fc-mrv (fill-a-pix->psr arr))))
		(cond ((equal res nil)
			nil)
			(T (psr->fill-a-pix res (array-dimension arr 0) (array-dimension arr 1))))))
	
			
;========================= FIM FUNCOES PARA RESOLUCAO CSP =================================================
(defvar puzzle1)
(defvar puzzle2)
(defvar puzzle5)
(defvar puzzle1.1)
(defvar puzzle0)
(defvar psr0)
(defvar psr5)
(defvar psr2)
(defvar psr1)
(defvar psr1.1)

(setf puzzle0 (make-array (list 3 3) :initial-contents 
	'((4 nil nil)
	  (nil nil nil)
	  (nil nil nil))))

(setf puzzle1 (make-array (list 5 5) :initial-contents
	'((nil nil 1 nil nil)
	  (nil 1 nil nil 5)
	  (1 nil nil nil 6)
	  (nil nil nil 9 nil)
	  (nil 5 6 nil nil))))

(setf puzzle1.1 (make-array (list 5 5) :initial-contents
	'((nil 2 3 nil nil)
	  (nil nil nil nil nil)
	  (nil nil 5 nil nil)
	  (nil 4 nil 5 nil)
	  (nil nil 4 nil nil)
	  )))
	  
(setf puzzle2 (make-array (list 10 10) :initial-contents
	'((nil 2 3 nil nil 0 nil nil nil nil)
	  (nil nil nil nil 3 nil 2 nil nil 6)
	  (nil nil 5 nil 5 3 nil 5 7 4)
	  (nil 4 nil 5 nil 5 nil 6 nil 3)
	  (nil nil 4 nil 5 nil 6 nil nil 3)
	  (nil nil nil 2 nil 5 nil nil nil nil)
	  (4 nil 1 nil nil nil 1 1 nil nil)
	  (4 nil 1 nil nil nil 1 nil 4 nil)
	  (nil nil nil nil 6 nil nil nil nil 4)
	  (nil 4 4 nil nil nil nil 4 nil nil))))

(setf puzzle5 (make-array (list 15 15) :initial-contents
	'((0 nil nil 4 3 2 1 nil nil nil nil nil 3 nil nil)
	  (nil nil 5 nil nil 4 nil nil 4 4 nil nil nil nil 3)
	  (nil 5 4 5 4 5 5 nil 5 3 nil 1 2 nil 3)
	  (4 nil nil nil 4 nil nil 4 2 nil 1 nil nil nil nil)
	  (nil nil 5 4 nil 2 2 nil 1 0 nil nil 7 5 nil)
	  (nil nil nil 5 nil nil 0 nil nil nil nil 4 5 nil 2)
	  (4 nil nil 5 4 2 0 0 nil nil nil 5 6 nil nil)
	  (5 nil nil 6 5 nil nil nil nil nil 3 3 3 nil 3)
	  (nil nil 5 nil 5 3 nil nil nil nil nil nil 3 nil nil)
	  (5 nil nil 6 5 nil 3 5 nil 6 nil nil 0 nil 0)
	  (nil nil 5 nil 4 3 2 4 5 nil 4 nil nil 1 nil)
	  (nil 7 nil nil 5 nil nil 1 nil 5 5 5 nil nil nil)
	  (nil nil 6 4 4 4 3 1 2 4 nil nil 6 4 nil)
	  (nil 5 nil 6 nil nil nil nil nil 4 6 nil nil nil nil)
	  (nil nil nil nil nil nil 3 2 0 nil 4 4 3 nil 2))))

(setf psr1.1 (fill-a-pix->psr puzzle1.1))
(setf psr2 (fill-a-pix->psr puzzle2))
(setf psr5 (fill-a-pix->psr puzzle5))
(setf psr1 (fill-a-pix->psr puzzle1))
(setf psr0 (fill-a-pix->psr puzzle0))
