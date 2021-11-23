runBrittany tabSize text = do
  let config' = staticDefaultConfig
      config  = config'
        { _conf_layout  = (_conf_layout config')
                            { _lconfig_indentAmount = coerce tabSize
                            }
        , _conf_forward = forwardOptionsSyntaxExtsEnabled
        }
  parsePrintModule config text
