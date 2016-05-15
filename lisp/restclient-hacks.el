;;; restclient-hacks --- Customizations to restclient

;;; Commentary:

;;; Code:

;; Hack so restclient-mode respects proxy settings.  This
;; only works on OS X.

(require 'restclient)

(defun networksetup-query (interface parameter)
  "Query the network setup for webproxy configuration.
Network `INTERFACE' to query.
`PARAMETER' to extract."
  (let ((command (format "networksetup -getwebproxy \"%s\" | grep \"^%s\" | awk '{print $2}' | xargs echo -n" interface parameter)))
         (shell-command-to-string command)))

(defun proxy-server (interface)
  "Get the proxy server for a given `INTERFACE'."
  (networksetup-query interface "Server"))

(defun proxy-port (interface)
  "Get the proxy port for a given `INTERFACE'."
  (networksetup-query interface "Port"))

(defun is-proxy-enabled (interface)
  "Determine if a proxy is enabled for the given `INTERFACE'."
  (let ((enabled (networksetup-query interface "Enabled")))
    (if (equal enabled "No") nil t)))

(defun restclient-http-do-customizations ()
  "Customize restclient."
  (make-local-variable 'url-proxy-services)
  (if (is-proxy-enabled "Wi-Fi")
    (let ((proxy-service (format "%s:%s" (proxy-server "Wi-Fi") (proxy-port "Wi-Fi"))))
      (setq url-proxy-services `(("http" . ,proxy-service) ("https" . ,proxy-service))))
    (setq url-proxy-services nil)))

(add-hook 'restclient-http-do-hook 'restclient-http-do-customizations)

(setq auto-mode-alist (cons '("\\.api$" . restclient-mode) auto-mode-alist))

(provide 'restclient-hacks)
;;; restclient-hacks.el ends here
