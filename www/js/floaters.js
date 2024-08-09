(function($) {

  $.floater = function(id, opt) {
    return new $.Floater(id, opt);
  }

  $.Floater = function(id, opt) {
    
    var $floater = $(id);
    var $this = this;
    
    this.floater = $floater;
    
    opt = $.extend({title: "Floater", cursor: "move", close: true, minimize: true, resize: false, width: 400, top: "50px", left: "50px"}, opt);
    
    $floater.css({width: opt.width});
    
    if(opt.hasOwnProperty("height")) {
      $floater.css({height: opt.height})
    }
    
    if(opt.hasOwnProperty("right")) {
      $floater.css({right: opt.right});
    } else {
      $floater.css({left: opt.left});
    }

    if(opt.hasOwnProperty("bottom")) {
      $floater.css({bottom: opt.bottom});
    } else {
      $floater.css({top: opt.top});
    }
    
    if(opt.resize) {
      $floater.css({resize: "both"});
    }

    $floater.addClass("float-panel closed")

    var $el = $floater.prepend("<div class = 'float-handle'>" + opt.title + "</div>").find(".float-handle");
    
    $el.css('cursor', "move").on("mousedown", function(e) {
      var $drag = $(this).addClass('active-handle').parent().addClass('draggable');
      var z_idx = $drag.css('z-index'),
          drg_h = $drag.outerHeight(),
          drg_w = $drag.outerWidth(),
          pos_y = $drag.offset().top + drg_h - e.pageY,
          pos_x = $drag.offset().left + drg_w - e.pageX;
    
    $(".float-panel").removeClass("on-top");
    
    $drag.addClass("on-top").parents().on("mousemove", function(e) {
      $('.draggable').offset({
          top:e.pageY + pos_y - drg_h,
          left:e.pageX + pos_x - drg_w
      }).on("mouseup", function() {
          $(this).removeClass('draggable');
      });
    });
    e.preventDefault(); // disable selection
    }).on("mouseup", function() {
      $(this).removeClass('active-handle').parent().removeClass('draggable');
    });
    
    $floater.on("click", function() {
      $(".float-panel").removeClass("on-top");
      $floater.addClass("on-top");
    })
    
    if(opt.minimize == true) {
      var min = $floater.append("<a class = 'minimize'><i class = 'fa fa-minus'></i></a>").find(".minimize");
      if(opt.close == true) {
        min.css({right: "25px"})
      } else {
        min.css({right: "5px"})
      }
      min.on("click", function() { 
        $floater.toggleClass("minimized").removeClass("open").removeClass("closed");
      })
    }
    
    if(opt.close == true) {
      var clo = $floater.append("<a class = 'close'><i class = 'fa fa-close'></i></a>").find(".close");
      clo.on("click", function() {$floater.addClass("closed").removeClass("open").removeClass("on-top").removeClass("minimized");})
    }

    this.open = function() {
      $(".float-panel").removeClass("on-top");
      $floater.addClass("open on-top").removeClass("closed").removeClass("minimized");
    }
    
    this.close = function() {
      $floater.addClass("closed").removeClass("open").removeClass("on-top").removeClass("minimized");
    }
    
    this.minimize = function() {
      $floater.toggleClass("minimized").removeClass("open").removeClass("closed");
    }
    
    this.updateTitle = function(title) {
      $floater.find(".float-handle").text(title)
    }
    
    return this;
    
  };
    
})(jQuery);

