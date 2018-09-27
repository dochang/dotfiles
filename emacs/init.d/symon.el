(req-package symon
  :custom
  (symon-delay 2)
  (symon-refresh-rate 1)
  (symon-monitors
   (append '(symon-current-time-monitor)
           (cond ((memq system-type '(gnu/linux cygwin))
                  '(symon-linux-memory-monitor
                    symon-linux-cpu-monitor
                    symon-linux-battery-monitor
                    symon-linux-network-rx-monitor
                    symon-linux-network-tx-monitor))
                 ((memq system-type '(darwin))
                  '(symon-darwin-memory-monitor
                    symon-darwin-cpu-monitor
                    symon-darwin-battery-monitor
                    symon-darwin-network-rx-monitor
                    symon-darwin-network-tx-monitor))
                 ((memq system-type '(windows-nt))
                  '(symon-windows-memory-monitor
                    symon-windows-cpu-monitor
                    symon-windows-battery-monitor
                    symon-windows-network-rx-monitor
                    symon-windows-network-tx-monitor))))))
