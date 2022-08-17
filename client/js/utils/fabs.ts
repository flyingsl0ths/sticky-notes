import Components from "./components.js";
import Optional from "./optional.js";

export type FABContainer = HTMLDivElement;
export type FABContainerAction = (container: FABContainer) => void;

function isFabContainer(elem: HTMLElement | null): elem is FABContainer {
    return elem !== null && elem.id === "fab-options";
}

function handleFABAction(action: string | undefined): Optional<() => void> {
    switch (action) {
        case "github":
            return new Optional(() => {
                window.open(
                    "https://github.com/flyingsl0ths/sticky-notes",
                    "_blank",
                    "noopener,noreferrer"
                );
            });

        case "post":
            return new Optional(() => {
                Components.showModal(Components.makePostModal());
            });

        default:
            return new Optional();
    }
}

function onFABClicked(target: HTMLElement) {
    const action = target.dataset["action"];

    handleFABAction(action).ifPresentOrElse(
        f => f(),
        () => console.error("Unknown button action")
    );
}

// function onFABElementClicked(target: HTMLElement) {
//     const parent = target.closest(".fab-option");
//
//     if (parent === null) {
//         console.error("Unable to find FAB parent container");
//         return;
//     }
//
//     onFABClicked(parent as HTMLElement);
// }

const M = {
    withFabContainer(
        container: HTMLElement | null,
        f: FABContainerAction
    ): void {
        if (!isFabContainer(container)) {
            console.error("div is not a FABContainer");
            return;
        }

        f(container);
    },

    onFABContainerClicked(container: FABContainer): void {
        container.addEventListener("pointerdown", event => {
            const toggled = container.dataset["toggled"];

            if (
                (toggled !== undefined && toggled === "0") ||
                toggled === undefined
            ) {
                return;
            }

            const target = event.target;

            if (target === null) {
                return;
            }

            const eventTarget = target as HTMLElement;

            const hasAction =
                eventTarget.parentElement?.dataset["action"] !== undefined;

            if (hasAction) {
                onFABClicked(eventTarget.parentElement);
            }
        });
    }
};

export default M;
