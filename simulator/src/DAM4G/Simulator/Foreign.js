/**
 * 
 * @param {HTMLElement} htmlEl 
 * @param {string} html
 * @returns 
 */
export const unsafeInnerHtmlImpl = (htmlEl, html) => {
  htmlEl.innerHTML = html;
}