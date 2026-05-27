const stripe = require('stripe')(process.env.STRIPE_SECRET_KEY);
const { createClient } = require('@supabase/supabase-js');

const supabase = createClient(
  process.env.SUPABASE_URL,
  process.env.SUPABASE_SERVICE_ROLE_KEY
);

exports.handler = async (event) => {
  if (event.httpMethod !== 'POST') {
    return { statusCode: 405, body: 'Method Not Allowed' };
  }

  const sig = event.headers['stripe-signature'];
  let stripeEvent;

  // ── Verify the webhook signature ─────────────────────────
  try {
    stripeEvent = stripe.webhooks.constructEvent(
      event.body,
      sig,
      process.env.STRIPE_WEBHOOK_SECRET
    );
  } catch (err) {
    console.error('Webhook signature verification failed:', err.message);
    return { statusCode: 400, body: `Webhook Error: ${err.message}` };
  }

  // ── Only handle checkout.session.completed ────────────────
  if (stripeEvent.type !== 'checkout.session.completed') {
    return { statusCode: 200, body: 'Event ignored' };
  }

  const session = stripeEvent.data.object;

  // ── Get product IDs from session metadata ─────────────────
  const productIds = session.metadata?.product_ids?.split(',').filter(Boolean);

  if (!productIds || productIds.length === 0) {
    console.error('No product_ids found in session metadata');
    return { statusCode: 400, body: 'No product IDs in metadata' };
  }

  // ── Decrement stock and clear reservation for each item ───
  for (const productId of productIds) {
    const { data: product, error: fetchError } = await supabase
      .from('products')
      .select('id, stock')
      .eq('id', productId)
      .single();

    if (fetchError || !product) {
      console.error(`Could not find product ${productId}:`, fetchError);
      continue;
    }

    const newStock = Math.max(0, product.stock - 1);

    const { error: updateError } = await supabase
      .from('products')
      .update({
        stock: newStock,
        reserved_until: null
      })
      .eq('id', productId);

    if (updateError) {
      console.error(`Failed to update stock for ${productId}:`, updateError);
    } else {
      console.log(`Stock updated for ${productId}: ${product.stock} → ${newStock}`);
    }
  }

  return { statusCode: 200, body: 'OK' };
};
