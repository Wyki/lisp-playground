(defun itemListStr (attributeValues)
	(if (eql (cdr attributeValues) NIL)
		(string (car attributeValues))
			(concatenate 'string (string (car attributeValues))
				";"
			(itemListStr (cdr attributeValues)))))

(defun propertiesStr (data)
	(if (null data)
		(values " />" nil)
		(if (listp (car data))
			(let ((singleProperty ""))
				(setf singleProperty (concatenate 'string " " (string (caar data)) "=\"" (itemListStr (cdar data)) "\"" ))
				(multiple-value-bind (returnStr returnLst)(propertiesStr (cdr data))
					(values (concatenate 'string singleProperty " " returnStr) returnLst)))
			(values ">" data))))
												
(defun contentStr (content)
	(if (null content)
		""
		(concatenate 'string (string (car content)) (contentStr (cdr content)))))

(defun html (tag &rest data)
	(concatenate 'string 
		"<" (string tag)
		(multiple-value-bind (strProp content) (propertiesStr data)
			(concatenate 'string 
				strProp
				(if (null content)
					""
					(concatenate 'string 
						(contentStr content)
						"</ " (string tag) ">"))))))


;(trace itemListStr propertiesStr contentStr html)


(set-macro-character #\[
(lambda (stream char)
 (let* ((lst
        (read-delimited-list #\]
     stream t)))
  (list* 'html lst))))


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
