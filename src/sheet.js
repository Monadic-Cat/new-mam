function range(start, end, step) {
	if (step === undefined) { step = 1; }
	let arr = [];
	for(let i = start; i < end; i += step) {
		arr.push(i);
	}
	return arr;
}

/**
 * makeLabelLine
 *
 * @param {object}	label - `label` object with `.name` and `.value` attributes.
 *
 * @return {string}
 *
 */
function makeLabelLine(label) {
	let line = `**${label.name}:** `;
	line +=
		range(-2, 4).map(x =>
			(x === label.value ? `**${x}**`:`${x}`)).join(" | ");
	return line;
}

/**
 * makeLabelSheet
 *
 * @param {array}	labels - List of labels (Freak, Danger, Savior, ...)
 * with `.name` and `.value` attributes.
 *
 * @return {string}
 *
 */
function makeLabelSheet(labels) {
	let new_labels = labels.order.map(l => ({ name: l, value: labels.dict[l] }));
	return new_labels.map(makeLabelLine).join("\n");
}
/**
 * Generate Markdown from conditions.
 * @param {array} conditions -
 * List of conditions (Hopeless, Insecure, ...) with `.name` and `.marked`
 * attributes.
 * @return {string}
 */
function makeConditionsLine(conditions) {
	let new_conditions = conditions.order.map(c => ({ name: c, marked: conditions[c] }));
	return new_conditions.map(x => x.marked ? `**${x.name}**`:`${x.name}`).join(" | ");
}


/**
 * groupPotential
 *
 * @param {number}	n - number of Potential
 * @param {number}	groupSize - Max size of groups
 *
 * @return {Array<number>}
 *
 */
function groupPotential(n, groupSize) {
	let groups = range(0, n / groupSize | 0).map(x => groupSize);
	groups.push(n % groupSize);
	return groups;
}

function makePotentialLine(potential, markedPotential, groupSize) {
	let pots = groupPotential(potential, groupSize);
	let marked = (markedPotential / groupSize) | 0;
	return pots.map((x, i) => (marked > i) ? `~~${x}~~`:`${x}`).join(" ");
}

/**
 * Generate Markdown from a character state.
 * @param {object} state - Character state
 *
 * @return {string} sheet
 */
function makeSheet(state) {
	return [
		`**Player Name:** ${state.player}`
		,`**Character Name:** ${state.name}`
		,`**Character Hero Name:** ${state.heroName}`,
		,`**Playbook:** ${state.playbook}`
		,`**Powers/Abilities:** ${state.powers}`
		,"**-----------------------------------**"
		,"**[Labels]**",
		,makeLabelSheet(state.labels),
		,`**Conditions:** ${makeConditionsLine(state.conditions)}`
		//,`**Harm:** ${state.harm}`
		,`**Potential:** ${makePotentialLine(state.potential, state.markedPotential, 5)}`
	].join("\n");
}

export default makeSheet;
export { groupPotential };
