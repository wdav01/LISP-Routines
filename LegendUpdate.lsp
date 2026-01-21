;;; LegendUpdate.lsp
;;; ------------------------------------------------------------
;;; LEGENDUPDATE - Scan DWG folder for placed blocks and rebuild legend.
;;;
;;; How to use:
;;; 1) APPLOAD this file in AutoCAD LT.
;;; 2) Run command: LEGENDUPDATE
;;; 3) Pick any DWG inside the folder to scan.
;;; 4) Pick the legend DWG to update.
;;; 5) Choose insertion point and optional spacing values.
;;;
;;; The routine logs activity to LegendUpdateLog.txt in the scan folder.
;;;
;;; Limitations:
;;; - Dynamic/anonymous blocks (*U###) are skipped because LT cannot reliably
;;;   resolve EffectiveName without COM.
;;; - Best-effort block import uses -INSERT "source=block" and may fail in
;;;   some versions or with unusual block definitions.
;;; ------------------------------------------------------------

(defun _folder-from-file (filepath)
  (if filepath
    (vl-filename-directory filepath)
  )
)

(defun _real-block-name-p (blkname)
  (and blkname
       (/= blkname "")
       (not (wcmatch blkname "*|*"))
       (not (wcmatch blkname "`**"))
  )
)

(defun _unique (lst / out)
  (setq out '())
  (foreach item lst
    (if (not (member item out))
      (setq out (cons item out))
    )
  )
  (reverse out)
)

(defun _sort-strings (lst)
  (if lst (vl-sort lst '<) '())
)

(defun _ensure-layer (lname / layerdata)
  (setq layerdata (tblsearch "LAYER" lname))
  (if (not layerdata)
    (command "._-LAYER" "_M" lname "_C" "7" "" "")
  )
  lname
)

(defun _ss-erase-layer (lname / ss)
  (setq ss (ssget "X" (list (cons 8 lname))))
  (if ss
    (command "._ERASE" ss "")
  )
)

(defun _log-open (logpath)
  (open logpath "w")
)

(defun _log (logfh msg)
  (if logfh
    (write-line msg logfh)
  )
)

(defun _log-close (logfh)
  (if logfh
    (close logfh)
  )
)

(defun _block-exists-p (blkname)
  (and blkname (tblsearch "BLOCK" blkname))
)

(defun _try-import-block (source-dwg blkname / importspec)
  (setq importspec (strcat source-dwg "=" blkname))
  (command "._-INSERT" importspec "0,0" "1" "1" "0")
  (command "._ERASE" "_L" "")
  (_block-exists-p blkname)
)

(defun _scan-current-dwg (logfh / ss idx ent data blkname blocks)
  (setq blocks '())
  (setq ss (ssget "X" '((0 . "INSERT"))))
  (if ss
    (progn
      (setq idx 0)
      (while (< idx (sslength ss))
        (setq ent (ssname ss idx))
        (setq data (entget ent))
        (setq blkname (cdr (assoc 2 data)))
        (if (_real-block-name-p blkname)
          (progn
            (setq blocks (cons blkname blocks))
            (_log logfh (strcat "  Found block: " blkname))
          )
        )
        (setq idx (1+ idx))
      )
    )
    (_log logfh "  No inserts found.")
  )
  (_unique blocks)
)

(defun _scan-folder (folder legendpath logfh / files result mapfile file blocks bname)
  (setq result '())
  (setq mapfile '())
  (setq files (vl-directory-files folder "*.dwg" 1))
  (foreach file files
    (if (/= (strcase (strcat folder "\\" file)) (strcase legendpath))
      (progn
        (_log logfh (strcat "Scanning: " file))
        (princ (strcat "\nScanning: " file))
        (command "._OPEN" (strcat folder "\\" file))
        (setq blocks (_scan-current-dwg logfh))
        (foreach bname blocks
          (if (not (member bname result))
            (progn
              (setq result (cons bname result))
              (setq mapfile (cons (cons bname (strcat folder "\\" file)) mapfile))
            )
          )
        )
        (command "._CLOSE" "_N")
      )
    )
  )
  (list (_sort-strings (_unique result)) mapfile)
)

(defun _getreal-default (prompt default / val)
  (setq val (getreal (strcat prompt " <" (rtos default 2 2) ">: ")))
  (if val val default)
)

(defun _getdist-default (prompt default / val)
  (setq val (getdist (strcat prompt " <" (rtos default 2 2) ">: ")))
  (if val val default)
)

(defun _build-legend (blocks blockmap layer / pt colw rowh texth count cols rows idx col row bx by blkname source ok label)
  (_ensure-layer layer)
  (_ss-erase-layer layer)
  (setq pt (getpoint "\nPick top-left point for legend: "))
  (if (not pt)
    (princ "\nNo point selected. Legend update canceled.")
    (progn
      (setq colw (_getdist-default "Column width" 50.0))
      (setq rowh (_getdist-default "Row height" 10.0))
      (setq texth (_getreal-default "Text height" 2.5))
      (setq count (length blocks))
      (setq cols (if (> count 20) 2 1))
      (setq rows (if (> cols 1) (1+ (fix (/ (1- count) cols))) count))
      (setvar "CLAYER" layer)
      (setq idx 0)
      (foreach blkname blocks
        (setq col (fix (/ idx rows)))
        (setq row (- idx (* col rows)))
        (setq bx (+ (car pt) (* col colw)))
        (setq by (- (cadr pt) (* row rowh)))
        (setq source (cdr (assoc blkname blockmap)))
        (if (not (_block-exists-p blkname))
          (if source
            (setq ok (_try-import-block source blkname))
            (setq ok nil)
          )
          (setq ok T)
        )
        (if ok
          (command "._-INSERT" blkname (list bx by 0.0) "1" "1" "0")
          (progn
            (setq label (strcat "[Missing block: " blkname "]"))
            (command "._TEXT" (list (+ bx (* 0.5 colw)) by 0.0) texth 0.0 label)
          )
        )
        (if ok
          (command "._TEXT" (list (+ bx (* 0.5 colw)) by 0.0) texth 0.0 blkname)
        )
        (setq idx (1+ idx))
      )
    )
  )
)

(defun c:LEGENDUPDATE (/ scanfile legendfile folder logpath logfh scanres blocks mapfile oldvars layer)
  (setq oldvars (list
                  (cons "CMDECHO" (getvar "CMDECHO"))
                  (cons "FILEDIA" (getvar "FILEDIA"))
                  (cons "OSMODE" (getvar "OSMODE"))
                  (cons "CLAYER" (getvar "CLAYER"))
                  (cons "SDI" (getvar "SDI"))
                )
  )
  (setvar "CMDECHO" 0)
  (setvar "FILEDIA" 1)
  (setvar "OSMODE" 0)
  (setvar "SDI" 1)
  (setq scanfile (getfiled "Select any DWG in folder to scan" "" "dwg" 0))
  (if (not scanfile)
    (progn
      (princ "\nNo scan file selected.")
      (foreach v oldvars (setvar (car v) (cdr v)))
      (exit)
    )
  )
  (setq legendfile (getfiled "Select legend DWG to update" "" "dwg" 0))
  (if (not legendfile)
    (progn
      (princ "\nNo legend file selected.")
      (foreach v oldvars (setvar (car v) (cdr v)))
      (exit)
    )
  )
  (setq folder (_folder-from-file scanfile))
  (setq logpath (strcat folder "\\LegendUpdateLog.txt"))
  (setq logfh (_log-open logpath))
  (_log logfh (strcat "Legend Update Log - " (rtos (getvar "DATE") 2 6)))
  (_log logfh (strcat "Scan folder: " folder))
  (_log logfh (strcat "Legend file: " legendfile))
  (princ (strcat "\nScanning folder: " folder))
  (setq layer "LEGEND_AUTOGEN")
  (setq scanres (_scan-folder folder legendfile logfh))
  (setq blocks (car scanres))
  (setq mapfile (cadr scanres))
  (princ (strcat "\nFound " (itoa (length blocks)) " unique blocks."))
  (_log logfh (strcat "Found " (itoa (length blocks)) " unique blocks."))
  (_log-close logfh)
  (command "._OPEN" legendfile)
  (princ "\nUpdating legend...")
  (_build-legend blocks mapfile layer)
  (princ (strcat "\nLegend update complete. Log: " logpath))
  (foreach v oldvars (setvar (car v) (cdr v)))
  (princ)
)

(princ "\nLEGENDUPDATE loaded. Type LEGENDUPDATE to run.")
(princ)
