// Beforeunload event to store values in localStorage before refresh
window.addEventListener("beforeunload", function() {
    console.log("Beforeunload event triggered");
    var numTimePoints = $("#num_time_points").val();
    var numAttributes = $("#num_attributes").val();
    var attributeNames = $("#attribute_names").val();
    var qMatrixChoice = $("input[name=q_matrix_choice]:checked").val();

    // Create an object with the values
    var valuesToSave = {
    num_time_points: num_time_points,
    num_attributes: num_attributes,
    attribute_names: attribute_names,
    q_matrix_choice: q_matrix_choice
    };

    // Convert the object to a JSON string
    var valuesJSON = JSON.stringify(valuesToSave);

    // Save the JSON string to local storage
    localStorage.setItem("saved_values", valuesJSON);
});

$(document).ready(function() {
    console.log("Getting values from localStorage on document ready");
    console.log("num_time_points:", localStorage.getItem("num_time_points"));
    console.log("num_attributes:", localStorage.getItem("num_attributes"));
    console.log("attribute_names:", localStorage.getItem("attribute_names"));

    // Read data from local storage
    var numTimePoints = localStorage.getItem("num_time_points");
    var numAttributes = localStorage.getItem("num_attributes");
    var attributeNames = localStorage.getItem("attribute_names");
    var qMatrixChoice = localStorage.getItem("q_matrix_choice");

    // Update UI elements with retrieved data
    $("#num_time_points").val(numTimePoints);
    console.log($("#num_time_points").val())
    $("#num_attributes").val(numAttributes);
    $("#attribute_names").val(attributeNames);

    if (qMatrixChoice !== null) {
    $("input[name=q_matrix_choice][value=" + qMatrixChoice + "]").prop("checked", true);
    }
    
});
