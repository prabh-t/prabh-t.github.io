function showImgBox(obj) {
    document.getElementById('largeImg').src = obj.src;
    document.getElementById('imgBox').style.visibility = 'visible';
    if(document.selection) document.selection.empty();
    if(window.getSelection) window.getSelection().removeAllRanges();
}

function hideImgBox(obj) {
    obj.style.visibility = 'hidden';
}
