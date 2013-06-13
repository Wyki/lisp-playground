(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))

(defun attrEndVal ()
  (concatenate 'string ">"))

(defun attributeValStr (attributeValues)
	(if (eql (cdr attributeValues) NIL)
		(string (car attributeValues))
		(concatenate 'string (string (car attributeValues)) 
			";"
			(paramstr (cdr attributeValues)))))

(defun attributesStr (attributes)
	(concatenate 'string " " (string (car attributes)) "=\"" 
		(attributeValStr (cdr attributes)) "\" "))
		
(defun contentStrVal (content)
	(if (null content)
		(concatenate 'string "" (string #\return) (string #\linefeed))
		(concatenate 'string 
			(car content)
			(contentStrVal (cdr content)))))
		
(defun contentStrAttr (content)
	(if (listp (car content))
		(concatenate 'string 
			(attributesStr (car content)) 
			(contentStrAttr (cdr content)))
		(if (null content)
			(attrEndVal)
			(concatenate 'string
				(attrEndVal)
				(contentStrVal content)))))
		
(defun contentStr (content)
	(if (listp (car content))
		(contentStrAttr content)
		(concatenate 'string 
			(attrEndVal)
			(contentStrVal content))))
		
			
(defun html (tag &rest content)
	(let* ((NL (concatenate 'string (string #\return) (string #\linefeed)))
		(NLTAB (concatenate 'string NL "    ")))
		(replace-all
			(if (null content)
				(concatenate 'string
					"<" (string tag) " />" )
				(concatenate 'string
					(string #\return) (string #\linefeed) "<" (string tag)
					(contentStr content)
					;(string #\return) (string #\linefeed)
					"</" (string tag) ">" ))
				NL NLTAB)))
		

;(trace attributeValStr attributesStr contentStrVal contentStrAttr contentStr html)


(set-macro-character #\[
(lambda (stream char)
 (let* ((lst
        (read-delimited-list #\]
     stream t)))
  (eval (concatenate 'list '(html) lst)))))


(defun testhtml ()
(princ (html :html
			(html :head
				(html :title "testtitel"))
			(html :body
				(html :h1 '(:id "erster Absatz") "It works!")))))
				
				
(defun testhtml2 ()
(html :html
			(html :head
				(html :title "testtitel"))
			(html :body
				(html :h1 '(:id "erster Absatz") "It works!"))))
				
(defun testhtml3 ()
	[:html
		[:head
			[:title "testtitel" ]]
		[:body
			[:h1 '(:id "absatz" "abschnitt") "It works!" ]]])
				
(testhtml)
