tar_meta_opts <- list(dom = "t",
                      ordering = FALSE,
                      scrollY = "600px",
                      pageLength = 100,
                      columnDefs = list(list(width = '100px',
                                             targets = "date"),
                                        list(width = '180px',
                                             targets = "name"),
                                        list(width = '75px',
                                             targets = c("time", "seconds")),
                                        list(className = 'dt-right',
                                             targets = "size"),
                                        list(render = DT::JS("function(data, type, row, meta) {",  #truncate string length
                                                             "return type === 'display' && data != null && data.length > 60 ?",
                                                             "'<span title=\"' + data + '\">' + data.substr(0, 60) + '...</span>' : data;",
                                                             "}"),
                                             targets = "warnings")
                      )
)

