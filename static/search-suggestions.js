import {h, htm, render, useState, useEffect, createContext, useContext} from "./preact.js"
const html = htm.bind(h)
const classNames = classArr => classArr.filter(el => el).join(" ")

const form = document.getElementById("bw-pr-search")

const AcceptSuggestion = createContext(null)
const hitsPromise = new Map()
const hitsDone = new Set()

function Suggestion(props) {
	const acceptSuggestion = useContext(AcceptSuggestion)
	return html`<li class="bw-ss__item"><button type="button" class="bw-ss__button" onClick=${() => acceptSuggestion(props)}>${props.title}</button></li>`
}

function fetchSuggestions(query, setSuggestions) {
	if (query === "") query = "\0"
	if (hitsPromise.has(query)) return hitsPromise.get(query)
	const url = new URL(`https://${BWData.wikiname}.fandom.com/api.php`)
	url.searchParams.set("action", "opensearch")
	url.searchParams.set("format", "json")
	url.searchParams.set("namespace", "0") // wiki namespace, 0 is default
	url.searchParams.set("origin", "*") // mediawiki api cors
	url.searchParams.set("search", query)
	const sendUrl = BWData.strict_proxy
			? "/proxy?" + new URLSearchParams({dest: url})
			: url
	const promise = fetch(sendUrl).then(res => res.json()).then(root => {
		hitsDone.add(query)
		return Array(root[1].length).fill().map((_, i) => ({
			title: root[1][i],
			url: root[3][i]
		}))
	})
	hitsPromise.set(query, promise)
	return promise
}

function SuggestionList(props) {
	return html`
<div class="bw-ss__container">
	<ul class=${classNames(["bw-ss__list", props.focus && "bw-ss__list--focus", `bw-ss__list--${props.st}`])}>
		${props.hits.map(hit => html`<${Suggestion} ...${hit} />`)}
	</ul>
</div>`
}

function ControlledInput() {
	const [query, setQuery] = useState("")
	const [focus, setFocus] = useState(false)
	const [st, setSt] = useState("ready")
	const [suggestions, setSuggestions] = useState([])

	useEffect(() => {
		if (st === "accepted") return
		setSt("loading")
		fetchSuggestions(query).then(s => {
			setSuggestions(s)
			if (hitsDone.size === hitsPromise.size) {
				setSt("ready")
			}
		})
	}, [query])

	function acceptSuggestion(suggestion) {
		setQuery(suggestion.title)
		setSt("accepted")
		const dest = new URL(suggestion.url).pathname.match("/wiki/.*")
		location = `/${BWData.wikiname}${dest}`
	}

	useEffect(() => {
		function listener(event) {
			if (event.type === "focusin") setFocus(true)
			else setFocus(false)
		}
		form.addEventListener("focusin", listener)
		form.addEventListener("focusout", listener)
		return () => {
			form.removeEventListener("focusin", listener)
			form.removeEventListener("focusout", listener)
		}
	})

	return html`
<${AcceptSuggestion.Provider} value=${acceptSuggestion}>
	<label for="bw-search-input">Search </label>
	<input type="text" name="q" id="bw-search-input" autocomplete="off" onInput=${e => setQuery(e.target.value)} value=${query} class=${classNames(["bw-ss__input", `bw-ss__input--${st}`])} />
	<${SuggestionList} hits=${suggestions} focus=${focus} st=${st}/>
</${AcceptSuggestion.Provider}`
}

render(html`<${ControlledInput} />`, form)
