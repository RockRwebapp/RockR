function animateCursor() {
    setInterval(function() {
        $("h1 #cursor").animate({
            opacity: 0
        }, "fast", "swing").animate({
            opacity: 1
        }, "fast", "swing")
    }, 600)
}

function animateTyping() {
    var t = captions[currCaption];
    if ($("h1 #verb").html(t.substr(0, captionLength++)), captionLength < t.length + 1) setTimeout("animateTyping()", 150);
    else {
        var e = 1200;
        "do" == captions[currCaption] && (e = 5e3), setTimeout("animateErasing()", e)
    }
}

function animateErasing() {
    var t = captions[currCaption];
    $("h1 #verb").html(t.substr(0, captionLength--)), captionLength >= 0 ? setTimeout("animateErasing()", 100) : (currCaption++, currCaption > captions.length - 1 && (currCaption = 0), animateTyping())
}

var captionLength = 0,
    captions = ["QFL", "Shepard", "Metamorphic Facies", "QAP", "TAS", "AFM", "Folk Sediment", "USDA Soils", "Mineral Classification"],
    currCaption = 0;
$(document).ready(function() {
setTimeout("animateCursor()", 2e3), setTimeout("animateTyping()", 3e3)
});
