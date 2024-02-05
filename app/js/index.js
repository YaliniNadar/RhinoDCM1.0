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

  
