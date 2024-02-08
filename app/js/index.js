export function goBack() {
    window.history.back();
}

function updateUI(inputId, value) {
    var element = document.getElementById(inputId);
    console.log(inputId);
    if (element) {
        var inputType = element.type;
        switch (inputType) {
            case 'checkbox':
            case 'radio':
                element.checked = value; // Set 'value' as boolean
                break;
            case 'select-multiple':
                // Handle multiple selection for select elements
                // Example: Set selected options based on 'value'
                break;
            default:
                element.value = value; // Update value attribute for text inputs, textareas, etc.
                console.log("updated value on UI")
                break;
        }
        // Pass the value to Shiny
        Shiny.setInputValue(inputId, value);
    } else {
        console.warn('Element with ID', inputId, 'not found or inaccessible.');
    }
}

export function readIndexedDBAndSave(dbName, inputIds, prefix) {
    var request = indexedDB.open(dbName);

    // Event handler for errors during IndexedDB opening
    request.onerror = function(event) {
        console.error('Error opening IndexedDB:', event.target.error);
    };

    // Event handler for success of IndexedDB opening
    request.onsuccess = function(event) {
        var db = event.target.result;
        console.log('IndexedDB:', db);

        var transaction = db.transaction('shinyStoresPlus', 'readonly');
        console.log('Database opened successfully');

        var objectStore = transaction.objectStore('shinyStoresPlus');
        console.log('Object store retrieved:', objectStore);

        var index = objectStore.index('var');

        for (var i = 0; i < inputIds.length; i++) {
            (function(inputId) { // Using an IIFE to create a closure
                var getRequest = index.get(prefix + inputId);
                getRequest.onsuccess = function(event) {
                    var result = event.target.result;
                    if (result) {
                        console.log('Found object for input ID', inputId, ':', result);
                        var value = result.value;
                        console.log('Value:', value);
                        updateUI(prefix + inputId, value);
                    } else {
                        console.log('No matching object found for input ID', prefix + inputId);
                    }
                };
            })(inputIds[i]); // Pass the current value of inputId into the IIFE
        }
    };
}

  
