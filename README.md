# template_R
easy handler for R manipulation <br>

## Examples <br>
*"pp.()"* function will be useful copy & paste tech when you don't feel like using bothersome read.csv() <br>
Others are; <br>

    plt.(iris[4:5])
    plt.(iris[-5], legePos = c(0.01, 0.99), lty = 1)
    dens.(iris[4:5], cum = F)
    crp.(iris[1:2])
    hist.(psd[1], binW = 0.1)
    corp.(iris[3:4])
    box2.(iris[-5], col = 1:4, rot = 22, cut = T)
    barp.(iris,xyChange=T,rot=25)
    barp.(iris,cum=T,xyChange=T)
    sp.(iris, col = 3)
    ...
