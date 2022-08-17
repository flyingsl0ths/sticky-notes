export type Dimensions = { width: number; height: number };
export type Position = { x: number; y: number };

const M = {
    getRandomNumber(max: number, rounded = true): number {
        const n = Math.random() * max;
        return rounded ? Math.floor(n) : n;
    },

    randomPosition(start: number, end: number): Position {
        const x = M.getRandomNumber(start);
        const y = M.getRandomNumber(end);
        return { x: x, y: y };
    },

    dimensionsOf(element: HTMLElement): Dimensions {
        const { width, height } = element.getBoundingClientRect();
        return { width, height };
    }
};

export default M;
