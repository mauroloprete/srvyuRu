function get_id(clicked_id) {

     var new_id = clicked_id.substring(0, clicked_id.indexOf("-") + 1) + "current_id";

     Shiny.setInputValue(new_id, clicked_id);
}

function remove_id(clicked_id) {
  var new_id = ''
  Shiny.setInputValue(new_id,clicked_id)
}