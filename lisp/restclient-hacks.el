;; Hack so restclient-mode respects proxy settings.  This
;; only works on OS X.

(defun networksetup-query (interface parameter)
  "Query the network setup for webproxy configuration."
  (let ((command (format "networksetup -getwebproxy \"%s\" | grep \"^%s\" | awk '{print $2}' | xargs echo -n" interface parameter)))
         (shell-command-to-string command)))

(defun proxy-server (interface) (networksetup-query interface "Server"))

(defun proxy-port (interface) (networksetup-query interface "Port"))

(defun is-proxy-enabled (interface)
  (let ((enabled (networksetup-query interface "Enabled")))
    (if (equal enabled "No") nil t)))

;; TODO: Update restclient to perform these checks before making a request,
;;       rather than relying on the buffer local variable.  Proxy settings
;;       could change during buffer's lifetime, but `url-proxy-settings'
;;       is only updated on switching the major mode.
(defun restclient-mode-customizations ()
  (message "Entering REST Client mode")
  (when (is-proxy-enabled "Wi-Fi")
    (let ((proxy-service (format "%s:%s" (proxy-server "Wi-Fi") (proxy-port "Wi-Fi"))))
      (make-local-variable 'url-proxy-services)
      (setq url-proxy-services `(("http" . ,proxy-service) ("https" . ,proxy-service))))))

(add-hook 'restclient-mode-hook 'restclient-mode-customizations)

(setq auto-mode-alist (cons '("\\.api$" . restclient-mode) auto-mode-alist))
