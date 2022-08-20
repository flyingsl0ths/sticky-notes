import type NoteRepository from "../NoteRepository.js";
import Components from "./components.js";
import Optional from "./optional.js";

export type FABContainer = HTMLDivElement;
export type FABContainerAction = (container: FABContainer) => void;

function isFabContainer(elem: HTMLElement | null): elem is FABContainer {
    return elem !== null && elem.id === "fab-options";
}

function handleFABAction(
    action: string | undefined,
    repo: NoteRepository
): Optional<() => void> {
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
                Components.showModal(Components.makePostModal(repo));
            });

        default:
            return new Optional();
    }
}

function onFABClicked(target: HTMLElement, repo: NoteRepository) {
    const action = target.dataset["action"];

    handleFABAction(action, repo).ifPresentOrElse(
        f => f(),
        () => console.error("Unknown button action")
    );
}

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

    toggle(source: HTMLElement | null, target: HTMLElement | null) {
        if (source === null && target === null) {
            return;
        }

        let toggle = true;
        (source as HTMLElement).addEventListener("pointerdown", () => {
            toggle = !toggle;

            const toggled = toggle ? "1" : "0";

            document.body.querySelectorAll(".fab").forEach(fab => {
                const button = fab as HTMLDivElement;
                button.style.cursor = toggle ? "pointer" : "default";
            });

            const fabContainer = target as HTMLDivElement;
            fabContainer.dataset["toggled"] = `${toggled}`;
            fabContainer.style.opacity = toggled;
        });
    },

    onFABContainerClicked(repo: NoteRepository): FABContainerAction {
        return (container: FABContainer) => {
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
                    onFABClicked(eventTarget.parentElement, repo);
                }
            });
        };
    }
};

export default M;
