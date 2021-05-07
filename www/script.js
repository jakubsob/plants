$(function() {
  $(".clickable-persona").on("click", function() {
    $(this).toggleClass("clicked");
    $(this).find("ms-Persona-primaryText").toggleClass("clicked");
    $(this).find("ms-Persona-secondaryText").toggleClass("clicked");
    let state = {
      "id": $(this).attr("id"),
      "active": $(this).hasClass("clicked")
    };
    Shiny.setInputValue("scrollable_personas_active", state);
  });
  
  $("#sidebar-select_all").on("click", function() {
    console.log("select_all");
    $(".clickable-persona").addClass("clicked");
    
  });
  
  $("#sidebar-clear_all").on("click", function() {
    $(".clickable-persona").removeClass("clicked");
  });
});

