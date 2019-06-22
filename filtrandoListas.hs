filtrandoListas :: [[a]]->[[a]]

menorC xss = minimum [length xs | xs <- xss]

filtrandoListas xss = [take menorT xs | xs <- xss]
       where menorT = menorC xss