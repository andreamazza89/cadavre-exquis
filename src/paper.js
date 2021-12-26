class Paper extends HTMLElement {
    constructor() {
        super();

        this.attachShadow({mode: 'open'});
        this.shadowRoot.innerHTML = `
        <style>
            .paper-container {
                width: 100%;
                min-height: 100vh;
                display: flex;
                justify-content: center;
            }

            .notepad {
                width: 80%;
                box-shadow: 10px 10px 40px rgba(0, 0, 0, .15);
                border-radius: 0 0 10px 10px;
                overflow: hidden;
                white-space: initial;
            }

            .top {
                width: 100%;
                height: 50px;
                background: #333;
                border-radius: 5px 5px 0 0;
            }

            .paper {
                width: 100%;
                height: 100%;
                min-height: 60vh;
                padding: 35px 20px;
                background: repeating-linear-gradient(#F1EDE9, #F1EDE9 71px, #94ACD4 71px, #94ACD4 72px);
                font-family: 'Shadows Into Light', cursive;
                line-height: 52px;
                outline: 0;
                font-size: 42px;
            }
            
            textarea {
                display: block;
                border: none;
                background-color: transparent;
                width: 90%;
                font-family: 'Shadows Into Light', cursive;
                line-height: 32px;
                outline: 0;
                font-size: 42px;
            }
            
            .slide-in-left{animation:slide-in-left .5s cubic-bezier(.25,.46,.45,.94) both}
            
            @keyframes slide-in-left{0%{transform:translateX(-1000px);opacity:0}100%{transform:translateX(0);opacity:1}}
        </style>
        
        <div class="paper-container">
            <div class="notepad slide-in-left">
                <div class="top"></div>
                <div id=${PAPER_SPACE} class="paper">
    <!--      the note's content gets added here using javascript below     -->
                </div>
            </div>
        </div>
        `
    }

    connectedCallback() {
        this.addPrompt()
        this.addHint()
        this.addHidden()
        this.addVisible()
    }

    addPrompt() {
        const prompt = document.createElement("p")
        prompt.textContent = "Tocca a te " + this.getAttribute("current-player")

        this.shadowRoot.getElementById(PAPER_SPACE).appendChild(prompt)
    }

    addHint() {
        if (this.getAttribute("show-hint") === 'true') {
            const hint = document.createElement("p")
            hint.textContent = this.getAttribute("hint-content")

            this.shadowRoot.getElementById(PAPER_SPACE).appendChild(hint)
        }
    }

    addHidden() {
        // This input is one-way only, in only 'tells' elm when something changes, but elm does not affect it.
        // So perhaps there's some sort of bug due to the fact that elm's state for this field and this field's internal
        // state could get out of sync.
        const hidden = document.createElement("textarea")
        hidden.placeholder = "Qua va la tua parte della storia nascosta"
        hidden.rows = 5

        hidden.oninput =
            () => this.dispatchEvent(new CustomEvent("hidden-content-changed", {detail: hidden.value}))

        this.shadowRoot.getElementById(PAPER_SPACE).appendChild(hidden)
    }

    addVisible() {
        if (this.getAttribute("show-visible") === 'true') {
            const visible = document.createElement("textarea")
            visible.placeholder = "Qua va la tua parte della storia che vedrà il prossimo"

            visible.oninput =
                () => this.dispatchEvent(new CustomEvent("visible-content-changed", {detail: visible.value}))

            this.shadowRoot.getElementById(PAPER_SPACE).appendChild(visible)
        }
    }

    addButton() {
        const button = document.createElement("input")
        button.type = "button"
        button.value = this.getAttribute("button-text")
        button.id = "paper-button"

        button.onclick =
            () => this.dispatchEvent(new CustomEvent("button-clicked"))

        this.shadowRoot.getElementById(PAPER_SPACE).appendChild(button)
    }

    removeButton() {
        this.shadowRoot.getElementById("paper-button").remove()
    }

    attributeChangedCallback(name, _oldValue, newValue) {
        this.checkButton(name, newValue)
    }

    checkButton(attributeName, shouldShowButton) {
        if (attributeName === SHOW_BUTTON) {
            if (shouldShowButton === "true") {
                this.addButton()
            } else {
                this.removeButton()
            }
        }
    }

    static get observedAttributes() {
        return [SHOW_BUTTON];
    }
}

const PAPER_SPACE = "paper-space"
const SHOW_BUTTON = "show-button"

window.customElements.define("paper-note", Paper);
