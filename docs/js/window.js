export default function loadWindow(windowName, googleAnalyticsId) {
    if (googleAnalyticsId && !(window.doNotTrack === "1" || navigator.doNotTrack === "1" || navigator.doNotTrack === "yes" || navigator.msDoNotTrack === "1")) {
        (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
            (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
            m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
        })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

        ga('create', googleAnalyticsId, 'auto');
        ga('send', 'pageview');
    }

    if (windowName) {
        window.name = windowName
    }

    window.addEventListener("beforeunload", savePosition);

    restorePosition();
}

function savePosition() {
    window.sessionStorage.setItem(window.location + "-scroll", window.scrollY);
}

function restorePosition() {
    if (!window.location.hash) {
        let scroll = window.sessionStorage.getItem(window.location + "-scroll");
        if (scroll) {
            setTimeout(function() {
                window.scrollTo(0, scroll);
            }, 100);
        }
    } else {
        setTimeout(function() {
            let h = document.querySelector(window.decodeURI(window.location.hash));
            if (h) {
                h.scrollIntoView();
            }
        }, 100);
    }
}
