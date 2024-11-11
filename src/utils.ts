export function appendObjectLocalStorage(key: string, data: Record<string, any>): void {
    const existingValue = localStorage.getItem(key);

    let newValue: Record<string, any>;

    if (existingValue !== null) {
        const oldValue = JSON.parse(existingValue) as Record<string, any>;

        newValue = { ...oldValue, ...data };
    } else {
        newValue = { ...data };
    }

    localStorage.setItem(key, JSON.stringify(newValue));
}

