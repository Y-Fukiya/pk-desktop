'use strict';

async function confirmDisplayedChoices(page) {
  for (const key of ['dosing', 'blq', 'c0', 'units']) {
    const status = page.locator(`#nca-review_${key} [data-review-state]`);
    const alreadyReviewed = status.locator('xpath=self::*[@data-review-state="confirmed" or @data-review-state="applied"]');
    const button = page.locator(`#nca-confirm_${key}`);
    await alreadyReviewed.or(button).waitFor();
    if (await button.isVisible()) await button.click();
    await alreadyReviewed.waitFor();
  }
}

async function chooseNcaPolicies(page, { dosing = 'ongoing', blq = 'exclude', c0 = 'none' } = {}) {
  await page.locator('#nca-dosing_context').selectOption(dosing);
  await page.locator('#nca-c0_method').selectOption(c0);
  if (blq) await page.locator('#nca-blq').selectOption(blq);
  await page.waitForFunction(policies => Object.entries(policies).every(([key, value]) =>
    !value || document.querySelector(`#nca-review_${key} [data-review-state]`)?.dataset.reviewValue === value), { dosing, c0, blq });
  await confirmDisplayedChoices(page);
}

module.exports = { chooseNcaPolicies, confirmDisplayedChoices };
