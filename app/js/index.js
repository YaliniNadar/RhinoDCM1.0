export function goBack() {
    window.history.back();
}

export function saveToLocalStorage(data) {
    for (var key in data) {
        if (data.hasOwnProperty(key)) {
            localStorage.setItem(key, data[key]);
        }
    }
}

export function getDefaultValue(key) {
    var storedValue = localStorage.getItem(key);
    console.log(storedValue);
}
  
