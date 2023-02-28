multi_katplotter = function(x){
  if(ncol(x) == 3){
    rowname1 = colnames(x)[1]
    rowname2 = colnames(x)[2]
    rowname3 = colnames(x)[3]
    rowname1 = sym(rowname1)
    rowname2 = sym(rowname2)
    rowname3 = sym(rowname3)
    p1 = ggplot(x, aes(x = !!rowname1)) + geom_bar()
    p2 = ggplot(x, aes(x = !!rowname2)) + geom_bar()
    p3 = ggplot(x, aes(x = !!rowname3)) + geom_bar()
    grid.arrange(p1, p2, p3, nrow = 3)
  }
  if(ncol(x) == 4){
    rowname1 = rownames(x)[1]
    rowname2 = rownames(x)[2]
    rowname3 = rownames(x)[3]
    rowname4 = rownames(x)[4]
    sym(rowname1)
    sym(rowname2)
    sym(rowname3)
    sym(rowname4)
    p1 = ggplot(x, aes(x = !!rowname1)) + geom_bar()
    p2 = ggplot(x, aes(x = !!rowname2)) + geom_bar()
    p3 = ggplot(x, aes(x = !!rowname3)) + geom_bar()
    p4 = ggplot(x, aes(x = !!rowname4)) + geom_bar()
    grid.arrange(p1, p2, p3, p4, nrow = 4)
  }
}
