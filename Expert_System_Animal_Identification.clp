;;;======================================================
;;;   Animal Identification Expert System - Project 1
;;;   By: Nicholas Pinney
;;;   Date: October 20, 2019
;;;
;;;     A simple expert system which attempts to identify
;;;     an animal based on its characteristics.
;;;     The knowledge base in this example is a 
;;;     collection of facts which represent backward
;;;     chaining rules. CLIPS forward chaining rules are
;;;     then used to simulate a backward chaining inference
;;;     engine.
;;;
;;;     CLIPS Version 6.4
;;; 
;;;     To execute, merely load, reset, and run.
;;;     Answer questions yes or no.
;;;======================================================

;;;***************************
;;;* DEFTEMPLATE DEFINITIONS *
;;;***************************

(deftemplate rule 
   (multislot if)
   (multislot then))

;;;**************************
;;;* INFERENCE ENGINE RULES *
;;;**************************

(defrule propagate-goal ""
   (goal is ?goal)
   (rule (if ?variable $?)
         (then ?goal ? ?value))
   =>
   (assert (goal is ?variable)))

(defrule goal-satified ""
   (declare (salience 30))
   ?f <- (goal is ?goal)
   (variable ?goal ?value)
   (answer ? ?text ?goal)
   =>
   (retract ?f)
   (format t "%s%s%n" ?text ?value))

(defrule remove-rule-no-match ""
   (declare (salience 20))
   (variable ?variable ?value)
   ?f <- (rule (if ?variable ? ~?value $?))
   =>
   (retract ?f))

(defrule modify-rule-match ""
   (declare (salience 20))
   (variable ?variable ?value)
   ?f <- (rule (if ?variable ? ?value and $?rest))
   =>
   (modify ?f (if ?rest)))

(defrule rule-satisfied ""
   (declare (salience 20))
   (variable ?variable ?value)
   ?f <- (rule (if ?variable ? ?value)
               (then ?goal ? ?goal-value))
   =>
   (retract ?f)
   (assert (variable ?goal ?goal-value)))

(defrule ask-question-no-legalvalues ""
   (declare (salience 10))
   (not (legalanswers $?))
   ?f1 <- (goal is ?variable)
   ?f2 <- (question ?variable ? ?text)
   =>
   (retract ?f1 ?f2)
   (format t "%s " ?text)
   (assert (variable ?variable (read))))

(defrule ask-question-legalvalues ""
   (declare (salience 10))
   (legalanswers ? $?answers)
   ?f1 <- (goal is ?variable)
   ?f2 <- (question ?variable ? ?text)
   =>
   (retract ?f1)
   (format t "%s " ?text)
   (printout t ?answers " ")
   (bind ?reply (read))
   (if (member$ (lowcase ?reply) ?answers) 
     then (assert (variable ?variable ?reply))
          (retract ?f2)
     else (assert (goal is ?variable))))

;;;***************************
;;;* DEFFACTS KNOWLEDGE BASE *
;;;***************************

(deffacts knowledge-base 
   (goal is type.animal)
   (legalanswers are yes no)
   (rule (if warm.blooded is yes)
         (then thing is warm))
   (rule (if warm.blooded is no)
         (then thing is cold))
   (question warm.blooded is "Is the animal warm blooded?")

   (rule (if thing is warm and 
          skin.fur is yes)
         (then skin is fur))
   (rule (if thing is warm and
          skin.fur is no)
         (then skin is feather))
   (question skin.fur is "Does the animal's skin have fur?")
   
   (rule (if skin is feather and 
          habitat.tree is yes)
         (then type.animal is sky.based.bird))
   (rule (if skin is feather and
          habitat.tree is no)
         (then type.animal is Penguin))
   (question habitat.tree is "Does the animal live in trees?")

   (rule (if skin is fur and 
         thing.predator is yes)
         (then state is predator))
   (rule (if skin is fur and
         thing.predator is no)
         (then state is pest))
   (question thing.predator is "Is the animal a predator?")

   (rule (if state is predator and 
         thing.tail is yes)
         (then tail is long.furry))
   (rule (if state is predator and
         thing.tail is no)
         (then type.animal is Manx-Cat))
   (question thing.tail is "Does the animal have a long and furry tail?")

   (rule (if tail is long.furry and 
         thing.habitat.jungle is yes)
         (then type.animal is Tiger))
   (rule (if tail is long.furry and
         thing.habitat.jungle is no)
         (then type.animal is Cat))
   (question thing.habitat.jungle is "Does the animal live in the jungle?")

   (rule (if warm.blooded is no)
         (then skin is scales))

   (rule (if skin is scales and 
         habitat.river is yes)
         (then type.animal is Salmon))
   (rule (if skin is scales and
         habitat.river is no)
         (then type.animal is Fish.In.The.Sea))
   (question habitat.river is "Does the animal live in rivers?")
   
   (rule (if state is pest and
         habitat.sewer is yes)
         (then type.animal is Rat))
   (rule (if state is pest and
         habitat.sewer is no)
         (then type.animal is Grey-Squirrel))
   (question habitat.sewer is "Does the animal live in sewers?")
   (answer is "I think your animal is a " type.animal)
)