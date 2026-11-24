# Recommendation rules

| Type | Trigger | Priority |
|---|---|---|
| Replenishment | Stock below forecast need over lead time + 14 days (plus safety stock) | Uses forecast priority; critical stockout is highest |
| Reduce purchasing | Stock above 90-day forecast, or dead or slow-moving stock | Medium for highest estimated holding-cost items; dead stock Medium; others Low |
| Festival stocking | Festival within 45 days and last year's demand 1.5x normal or more | High at 3x or more; else Medium |
| Customer retention | High churn risk and spend in the top quarter | High for the top 10% of spend; else Medium |
| Pricing review | Revenue in the top quarter and margin in the bottom quarter | High for the top 10% of revenue; else Medium |
| Investigate sales drop | Unusually low sales in the last 30 days | High when source severity is High; else Medium |
| Investigate returns | Return rate in the top 10% of products | High at 20% or more; else Medium |
| Supplier review | Lead time in the longest quarter and items needing replenishment | High at 20 or more items; else Medium |

Assumptions: holding cost 20% a year; 5-percentage-point margin improvement from a price or cost review;
negative sales quantities are treated as returns; cost prices, stock, stores, lead times and promotions may be simulated.
Return value is sales value exposed, not confirmed financial loss. These are decision-support suggestions,
not guaranteed outcomes.
