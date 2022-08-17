type Value<T> = T | null | undefined;

function isValid<T>(v: Value<T>): v is T {
    return v !== null && v !== undefined;
}

export default class Optional<T> {
    constructor(value: Value<T> = null) {
        if (new.target !== Optional<T>) {
            throw new Error("Subclassing is not allowed");
        }

        this.value = value;
    }

    ifPresent(f: (value: T) => void) {
        const value = this.value;

        if (isValid(value)) {
            f(value);
        }
    }

    ifPresentOrElse(f: (value: T) => void, g: () => void): void {
        const value = this.value;

        if (isValid(value)) {
            f(value);
        } else {
            g();
        }
    }

    isPresent(): boolean {
        return !!this.value;
    }

    private readonly value: Value<T>;
}
