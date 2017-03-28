function display(){
    $.ajax({
        url: "http://213.188.154.144/diff?v=x&e=" + encodeURIComponent($("#ExpressionInput").val())
    }).done(function(data){
        $("#ResultDiv").html("$\\frac{d}{dx}f(x)=" + data + "$")
        MathJax.Hub.Queue(["Typeset",MathJax.Hub,"ResultDiv"]);
    })
}
