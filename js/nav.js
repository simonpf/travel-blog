(function($) {
    "use strict";

    var $navbar = $("#header"),
        y_pos = $navbar.offset().top,
        height = $navbar.height();
    console.log("hallo");
    $(document).scroll(function() {
        var scrollTop = $(this).scrollTop();

        if (scrollTop > y_pos + height) {
            $navbar.addClass("navbar-fixed").animate({
                top: 0
            });
        } else if (scrollTop <= y_pos) {
            $navbar.removeClass("navbar-fixed").clearQueue().animate({
                top: "0px"
            }, 0);
        }
    });
})(jQuery, undefined);
