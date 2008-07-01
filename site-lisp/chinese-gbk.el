;;; chinese-gbk.el --- Chinese GBK support

;; Copyright (C) 2003  Xinwei Hu <huxw@knight.6test.edu.cn>

;; Author: Xinwei Hu <huxw@knight.6test.edu.cn>
;; Based on the work by Su Yong <ysu@mail.ustc.edu.cn>
;; Keywords: multilingual, Chinese, GBK

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; TODO:
;;; 1. Recheck the coding mapping with GB2312 and GBK. Some character
;;;    seems incorrect in GBK mode.

;;; ChangeLog:
;;; 5/16/2003              Initialization
;;; 5/21/2003              Unicode<->GBK mapping added

;;; Installation:

;;; Copy this file to your site-lisp directory and add following
;;; lines to your .emacs file

;;; (load "chinese-gbk")
;;; (set-keyboard-coding-system 'chinese-gbk)

;;; Commentary:

;;; Configuration mentioned above is enough for most
;;; environment. Only if you do input gbk only characters, you
;;; should save buffer in chinese-gbk coding system. And because
;;; most Emacs addons do not aware of chinese-gbk coding system,
;;; you'd better set your language environment to Chinese-GB to
;;; avoid some mule-related problems.

;;; Known Bugs:

;;; 1. While extract mail to file in gnus, chinese-gbk will
;;;    lose data unexpectedly

;;; Fixed Problem:
;;; 1. Problem with utf-translate-cjk-mode. This is nothing
;;;    related to my coding, by a typo in .emacs, mistaken
;;;    "Chinese" by "Ghinese" ;-(

;;; Code:

(require 'mule)
(require 'ccl)

(if (string= window-system "w32")
    (setq w32-charset-info-alist (cons '("gbk" w32-charset-gb2312 . 936) w32-charset-info-alist)))

(define-ccl-program ccl-decode-chinese-gbk
  '(2
    ((loop
      (read r0)
      ;; no constrain
      (if (r0 < ?\x80)
	  ;; r0 < 0x80, single byte char, write through
	  (write r0)
	;; r0 >= 0x80
	(if (r0 > ?\x80)
	    ;; r0 >= 0x81
	    (if (r0 == ?\xFF)
		;; r0 == 0xFF
		((write r0)
		 (repeat))
	      ;; 0x81 <= r0 <= 0xFE
	      (
	       (read r1)
	       (if (r1 < ?\x40)
		   ;; 0x81 <= r0 <= 0xFE, r1 < 0x40
		   (write r0 r1)
		 ;; 0x81 <= r0 <= 0xFE, r1 >= 0x40
		 (if (r1 <= ?\xFE)
		     ;; 0x81 <= r0 <= 0xFE, 0x40 <= r1 <= 0xFE
		     (if (r1 < ?\xA1)
			 ;; 0x81 <= r0 <= 0xFE, 0x40 <= r1 < 0xA1
			 (if (r1 == ?\x7F)
			     ;; 0x81 <= r0 <= 0xFE, r1 == 0x7F
			     ((write r0 r1)
			      (repeat))
			   ;; 0x81 <= r0 <= 0xFE, 0x40 <= r1 <= 0xA0. r1 != 0x7F
			   (if (r1 < ?\x7F)
			       ;; 0x81 <= r0 <= 0xFE, 0x40 <= r1 < 0x7F
			       ;; area 1
			       (
;;				(r1 = ((((r0 - ?\x81) * 63) + (r1 - ?\x40)) // 94))
				(r2 = (r0 - ?\x81))
				(r2 *= 63)
				(r3 = (r1 - ?\x40))
				(r2 += r3)
				(r1 = (r2 // 94))
;;				(r1 = ((((r1 + ?\x21) & ?\x7F) << 7) | ((r7 + ?\x21) & ?\x7F)))
				(r2 = (r1 + ?\x21))
				(r2 &= ?\x7F)
				(r2 <<= 7)
				(r3 = (r7 + ?\x21))
				(r3 &= ?\x7F)
				(r1 = (r2 | r3))
;;				(r0 = ,(charset-id 'chinese-cns11643-5))
				(r0 = 248)
				(write-multibyte-character r0 r1)
				)
			     ;; 0x81 <= r0 <= 0xFE, 0x7F < r1 <= 0xA0
			     ;; area 2
			     (
;;			      (r1 = ((((r0 - ?\x81) * 33) + (r1 - ?\x80)) // 94))
			      (r2 = (r0 - ?\x81))
			      (r2 *= 33)
			      (r3 = (r1 - ?\x80))
			      (r2 += r3)
			      (r1 = (r2 // 94))
;;			      (r1 = ((((r1 + ?\x21) & ?\x7F) << 7) | ((r7 + ?\x21) & ?\x7F)))
			      (r2 = (r1 + ?\x21))
			      (r2 &= ?\x7F)
			      (r2 <<= 7)
			      (r3 = (r7 + ?\x21))
			      (r3 &= ?\x7F)
			      (r1 = (r2 | r3))
;;			      (r0 = ,(charset-id 'chinese-cns11643-6))
			      (r0 = 249)
			      (write-multibyte-character r0 r1)
			      )
			    )
			  )
		       ;; 0x81 <= r0 <= 0xFE, 0xA1 <= r1 <= 0xFE
		       (if (r0 < ?\xA1) 
			   ;; 0x81 <= r0 < 0xA1, 0xA1 <= r1 <= 0xFE
			   ;; area 3
			   ((r0 -= ?\x60)
			    (r1 -= ?\x80)
;;			    (r1 = (((r0 & ?\x7F) << 7) | (r1 & ?\x7F)))			   
			    (r0 &= ?\x7F)
			    (r0 <<= 7)
			    (r1 &= ?\x7F)
			    (r1 |= r0)
;;			    (r0 = ,(charset-id 'chinese-cns11643-7))
			    (r0 = 250)
			    (write-multibyte-character r0 r1)
			    )
			 ;; 0xA1 <= r0 <= 0xFE, 0xA1 <= r1 <= 0xFE, this is gb2312 coding
			 ;; area 4
			 (
;;			  (r1 = (((r0 & ?\x7F) << 7) | (r1 & ?\x7F)))
			  (r0 &= ?\x7F)
			  (r0 <<= 7)
			  (r1 &= ?\x7F)
			  (r1 |= r0)
;;			  (r0 = ,(charset-id 'chinese-gb2312))
			  (r0 = 145)
			  (write-multibyte-character r0 r1)
			  )
			 ))
		   ;; 0x81 <= r0 <= 0xFE, r1 == 0xFF
		   ((write r0 r1)
		    (repeat)))
		 )))
	  ;; r0 = 0x80
	  ((write r0)
	   (repeat))
	  ))
      (repeat))))
  "CCL Program to decode GBK.")

(define-ccl-program ccl-encode-chinese-gbk
  `(1
    ((loop
      (read-multibyte-character r0 r1)
      (if (r0 == ,(charset-id 'ascii))
          (write r1)

		(if (r0 == ,(charset-id 'chinese-gb2312))
			( 
;;			 (r0 = ((r1 & ?\x7F) + ?\x80))
			 (r0 = (r1 & ?\x7F))
			 (r0 += ?\x80)
;;			 (r1 = (((r1 & 16256) >> 7) + ?\x80))
			 (r1 &= 16256)
			 (r1 >>= 7)
			 (r1 += ?\x80)
			 (write r1 r0))

		  (if (r0 == ,(charset-id 'chinese-cns11643-5))
		      (
;;		       (r0 = ((((((r1 & 16256) >> 7) - ?\x21) * 94) + ((r1 & ?\x7F) - ?\x21)) // 63))
		       (r2 = (r1 & 16256))
		       (r2 >>= 7)
		       (r2 -= ?\x21)
		       (r2 *= 94)
		       (r3 = (r1 & ?\x7F))
		       (r3 -= ?\x21)
		       (r2 += r3)
		       (r0 = (r2 // 63))
		       (r0 = (r0 + ?\x81))
		       (r1 = (r7 + ?\x40))
		       (write r0 r1)
		       )
			
		    (if (r0 == ,(charset-id 'chinese-cns11643-6))
			(
;;			 (r0 = ((((((r1 & 16256) >> 7) - ?\x21) * 94) + ((r1 & ?\x7F) - ?\x21)) // 33))
			 (r2 = (r1 & 16256))
			 (r2 >>= 7)
			 (r2 -= ?\x21)
			 (r2 *= 94)
			 (r3 = (r1 & ?\x7F))
			 (r3 -= ?\x21)
			 (r2 += r3)
			 (r0 = (r2 // 33))

			 (r0 = (r0 + ?\x81))
			 (r1 = (r7 + ?\x80))
			 (write r0 r1)
			 )
			  
		      (if (r0 == ,(charset-id 'chinese-cns11643-7))
			  (
;;			   (r0 = (((r1 & 16256) >> 7) + ?\x60))
			   (r0 = (r1 & 16256))
			   (r0 >>= 7)
			   (r0 += ?\x60)
;;			   (r1 = ((r1 & ?\x7F) + ?\x80))
			   (r1 &= ?\x7F)
			   (r1 += ?\x80)
			   (write r0 r1)
			   )
;;			(write r1)
			)))))
      (repeat))))
  "CCL program to encode into GBK.")


(make-coding-system
 'chinese-gbk 4 ?Z
 "Chinese GBK encoding for GBK characters mainly used
on Chinses PCs."
 
 '(ccl-decode-chinese-gbk . ccl-encode-chinese-gbk)
 '((safe-charsets
    ascii
	chinese-gb2312
	chinese-cns11643-5
	chinese-cns11643-6
	chinese-cns11643-7)
   (valid-codes (0 . 254))
   (mime-charset . gbk)
   ))

(define-coding-system-alias 'gbk 'chinese-gbk)

(set-language-info-alist
 "Chinese-GBK" '((coding-system chinese-gbk chinese-iso-7bit)
                 (coding-priority chinese-gbk iso-2022-cn chinese-iso-8bit)
                 (input-method . "chinese-py-ucdos")
                 (sample-text . "Chinese GBK ÄãºÃ")
                 (documentation . "Support for Chinese GBK character set."))
 '("Chinese"))

(require 'fontset)

(let ((l `((chinese-cns11643-5 . ("*" . "gbk"))
           (chinese-cns11643-6 . ("*" . "gbk"))
           (chinese-cns11643-7 . ("*" . "gbk"))
           ))
      charset font-spec arg)
  (while l
    (setq charset (car (car l)) font-spec (cdr (car l)) l (cdr l))
    (if (symbolp charset)
        (setq arg (make-char charset))
      (setq arg charset))
    (set-fontset-font "fontset-default" arg font-spec)
    (set-fontset-font "fontset-standard" arg font-spec)))


;; Create standard fontset from 16 dots fonts which are the most widely
;; installed fonts.  Fonts for Chinese-GB, Korean, and Chinese-CNS are
;; specified here because FAMILY of those fonts are not "fixed" in
;; many cases.
(defvar standard-fontset-spec
  (purecopy "-*-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-standard,
	chinese-gb2312:-*-medium-r-normal-*-16-*-gb2312*-*,
	korean-ksc5601:-*-medium-r-normal-*-16-*-ksc5601*-*,
	chinese-cns11643-5:-*-medium-r-normal-*-16-*-gbk*-0,
	chinese-cns11643-6:-*-medium-r-normal-*-16-*-gbk*-0,
	chinese-cns11643-7:-*-medium-r-normal-*-16-*-gbk*-0")
  "String of fontset spec of the standard fontset.
You have the biggest chance to display international characters
with correct glyphs by using the standard fontset.
See the documentation of `create-fontset-from-fontset-spec' for the format.")

(define-ccl-program ccl-encode-gbk-font
  '(0
    ;; In:  R0:chinese-gbk-1 or chinese-gbk-2
    ;;      R1:position code 1
    ;;      R2:position code 2
    ;; Out: R1:font code point 1
    ;;      R2:font code point 2
;;    (if (r0 == ,(charset-id 'ascii))
    (if (r0 == 0)
	(
	 (r2 = r1)
	 (r1 = 0))
;;      (if (r0 == , (charset-id 'chinese-gb2312))
      (if (r0 == 145)
	  (
	   (r1 += ?\x80)
	   (r2 += ?\x80))
;;	(if (r0 == , (charset-id 'chinese-cns11643-5))
	(if (r0 == 248)
	    (
;;	     (r1 = ((((r1 - ?\x21) * 94) + (r2 - ?\x21)) // 63))
	     (r1 -= ?\x21)
	     (r1 *= 94)
	     (r2 -= ?\x21)
	     (r1 += r2)
	     (r1 //= 63)
	     (r1 += ?\x81)
	     (r2 = (r7 + ?\x40)))
;;	  (if (r0 == ,(charset-id 'chinese-cns11643-6))
	  (if (r0 == 249)
	      (
;;	       (r1 = ((((r1 - ?\x21) * 94) + (r2 - ?\x21)) // 33))
	       (r1 -= ?\x21)
	       (r1 *= 94)
	       (r2 -= ?\x21)
	       (r1 += r2)
	       (r1 //= 33)
	       (r1 += ?\x81)
	       (r2 = (r7 + ?\x80)))
;;	    (if (r0 == ,(charset-id 'chinese-cns11643-7))
	  (if (r0 == 250)
		(
		 (r1 += ?\x60)
		 (r2 += ?\x80)
		 ) ; part 3
            (end))))))))

(setq font-ccl-encoder-alist
      (cons '("gbk" . ccl-encode-gbk-font)
	    font-ccl-encoder-alist))

(setq x-pixel-size-width-font-regexp
      (concat x-pixel-size-width-font-regexp "\\|gbk"))

(setq vertical-centering-font-regexp
      (concat vertical-centering-font-regexp "\\|gbk"))

(let ((gbk-list '(chinese-cns11643-5
		  chinese-cns11643-6
		  chinese-cns11643-7))
      generic-charset)
  (while gbk-list
    (setq generic-charset (car gbk-list))

    (modify-syntax-entry (make-char generic-charset) "w")
    (modify-syntax-entry (make-char generic-charset 33) "_")
    (modify-syntax-entry (make-char generic-charset 34) "_")
    (modify-syntax-entry (make-char generic-charset 41) "_")
    (modify-syntax-entry ?\¡² "(¡³")
    (modify-syntax-entry ?\¡´ "(¡µ")
    (modify-syntax-entry ?\¡¶ "(¡·")
    (modify-syntax-entry ?\¡¸ "(¡¹")
    (modify-syntax-entry ?\¡º "(¡»")
    (modify-syntax-entry ?\¡¼ "(¡½")
    (modify-syntax-entry ?\¡¾ "(¡¿")
    (modify-syntax-entry ?\¡³ ")¡²")
    (modify-syntax-entry ?\¡µ ")¡´")
    (modify-syntax-entry ?\¡· ")¡¶")
    (modify-syntax-entry ?\¡¹ ")¡¸")
    (modify-syntax-entry ?\¡» ")¡º")
    (modify-syntax-entry ?\¡½ ")¡¼")
    (modify-syntax-entry ?\¡¿ ")¡¾")

    (modify-category-entry (make-char generic-charset) ?c)
    (modify-category-entry (make-char generic-charset) ?\|)
    (modify-category-entry (make-char generic-charset 35) ?A)
    (modify-category-entry (make-char generic-charset 36) ?H)
    (modify-category-entry (make-char generic-charset 37) ?K)
    (modify-category-entry (make-char generic-charset 38) ?G)
    (modify-category-entry (make-char generic-charset 39) ?Y)
    (let ((row 48))
      (while (< row 127)
        (modify-category-entry (make-char generic-charset row) ?C)
        (setq row (1+ row))))
    (setq gbk-list (cdr gbk-list))))

(define-minor-mode utf-translate-gbk-mode
  "Whether the UTF based coding systems should decode/encode GBK characters.
Enabling this loads tables which allow the coding systems
mule-utf-8, mule-utf-16-le and mule-utf-16-be to encode
characters in the charsets `chinese-gbk', and to decode the
corresponding unicodes into such characters.

The tables are large (over 40000 entries), so this option is not the
default.  Also, installing them may be rather slow."
  :init-value nil
  :version "21.4"
  :type 'boolean
  :group 'mule
  :global t
  (if utf-translate-gbk-mode
      ;; Fixme: Allow the use of the CJK charsets to be
      ;; customized by reordering and possible omission.
      (progn
	;; Redefine them with realistic initial sizes and a
	;; smallish rehash size to avoid wasting significant
	;; space after they're built.
	(setq ucs-mule-gbk-to-unicode
	      (make-hash-table :test 'eq :size 43000 :rehash-size 1000)
	      ucs-unicode-to-mule-gbk
	      (make-hash-table :test 'eq :size 43000 :rehash-size 1000))
	;; Load the files explicitly, to avoid having to keep
	;; around the large tables they contain (as well as the
	;; ones which get built).
	(load "subst-gbk")
	(define-translation-hash-table 'utf-subst-table-for-decode
	  ucs-unicode-to-mule-gbk)
	(define-translation-hash-table 'utf-subst-table-for-encode
	  ucs-mule-gbk-to-unicode)
	(set-char-table-extra-slot (get 'utf-translation-table-for-encode
					'translation-table)
				   1 ucs-mule-gbk-to-unicode))
    (define-translation-hash-table 'utf-subst-table-for-decode
      (make-hash-table :test 'eq))
    (define-translation-hash-table 'utf-subst-table-for-encode
      (make-hash-table :test 'eq))
    (set-char-table-extra-slot (get 'utf-translation-table-for-encode
				    'translation-table)
			       1 nil)))

;;; chinese-gbk.el ends here
